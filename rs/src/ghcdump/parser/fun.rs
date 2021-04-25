use regex::Regex;
use lazy_static::lazy_static;
use std::path;
use std::collections::HashMap;
use std::fmt;

use super::{Parser, ErrPos};
use super::errpos;

type Error = super::Error;

#[derive(Debug)]
pub enum Reason {
    GlobalNotFound,
    LocalNotFound,
}

// Export

pub type Prod = HashMap<Global, Result<Decl, String>>;

pub fn parse(input: &str, file_name: &path::Path) -> Prod
{
    let mut parser = Parser::new(input);
    let mut declarations: Prod = HashMap::new();
    let verbose_replace = false;

    let mut insert_val = |key, value| {
        match (declarations.get(&key), &value) {
            (None, _) => {
                let _ = declarations.insert(key, value);
            }

            (Some(Err(prev_err)), Ok(_decl)) => {
                if verbose_replace {
                    println!("Ignored error {}", prev_err);
                }
                let _ = declarations.insert(key, value);
            }

            (Some(Err(prev_err)), Err(_new_err)) => {
                /* Ignore the previous error and replace it
                 * with the new one.
                 */
                if verbose_replace {
                    println!("Ignored error {}", prev_err);
                }
                let _ = declarations.insert(key, value);
            }

            (Some(Ok(_decl)), Err(new_err)) => {
                /* Ignore the error and keep the value */
                if verbose_replace {
                    println!("Ignored error {}", new_err);
                }
            }

            (Some(Ok(_prev_decl)), Ok(_new_decl)) => {
                /* Duplicate declaration: this shouldn't happen.
                 * May happen with locals though.
                 *
                 * For now, we keep the current value.
                 */
                if verbose_replace {
                    println!("Duplicate definition found for {}", key);
                }
            }
        }
    };

    while parser.has_input_left() {
        let parsed_global = parse_global(&mut parser);
        match parsed_global {
            /* There is a global symbol.
             * Try to parse a declaration.
             */
            Ok(symbol) => {
                let parsed_decl = parse_decl(&mut parser, &symbol);
                match parsed_decl {
                    /* Successful parsed a declaration.
                     * Store it in the hashmap.
                     */
                    Ok (decl) => {
                        /* TODO use parser.finalize here */
                        insert_val(symbol, Ok(decl))
                    },

                    /* Couldn't parse a declaration */
                    Err(err) => {
                        if ignore_decl_error(&err, input) {
                            /* Silently ignore this error */
                        }
                        else {
                            /* Format the error report and remember
                             * it in the HashMap.
                             */
                            let gen_report = format_parsing_error(&err, input, file_name);
                            match gen_report {
                                Ok(report) => {
                                    insert_val(symbol, Err(report))
                                }

                                Err(fatal_err) =>
                                    println!("{}", fatal_err),
                            }
                        }
                        /* In both cases, skip the current line */
                        skip_after_empty_line(&mut parser);
                    },
                }
            },

            /* We couldn't even parse a symbol at the beginning of the line.
             * Give up on parsing this declaration.
             */
            Err(_) =>
                skip_after_empty_line(&mut parser),
        }
    }

    declarations
}

fn skip_after_empty_line(parser: &mut Parser) {
    fn should_skip_again(input: &str) -> bool {
        super::str_first(input)
            .map(|c| c == ' ')  /* Dump files indented by space */
            .unwrap_or(false)   /* end of file: avoid infinite loop */
    }

    /* Skip lines until we reach unindented code.
     * This means skipping lines until the first char is not a space.
     *
     * Note: trying to match an empty line will fail overall because
     * the type signature is just above the declaration. So the entire block
     * signature + declaration would be skipped, and we wouldn't parse anything.
     */
    parser.skip_curr_line();
    while parser.peek(should_skip_again)
    {
        parser.skip_curr_line();
    }
}

pub fn merge(file_prods: Vec<Prod>) -> Prod {
    let mut final_res = HashMap::new();

    for file_decls in file_prods.into_iter() {
        for (new_key, new_value) in file_decls.into_iter() {
            print!("{} ", &new_key);
            if final_res.contains_key(&new_key) {
                println!("Conflicting definitions for {} in different files",
                        new_key);
            }
            else {
                let dup = final_res.insert(new_key, new_value);
                assert!(dup.is_none());
            }
        }
    }
    println!("");

    return final_res;
}

// Decl

#[derive(Debug)]
pub struct Decl {
    name: Global,
    body: Expr
}

fn parse_decl(parser: &mut Parser, symbol: &Global) -> Result<Decl, Error> {
    let fun_name = symbol.clone();
    match_keyword(parser, "=")?;
    let body = parse_expression(parser)?;
    Ok(Decl {
        name: fun_name,
        body: body
    })
}

fn match_keyword(parser: &mut Parser, keyword: &str) -> Result<(), Error> {
    parser.match_keyword(keyword)
}

// Expr

/* TODO consider type Expr = Box<Expr_> */
#[derive(Debug)]
enum Expr {
    AnonFun(AnonFun),
    FunCall(FunCall),
}

fn parse_expression(parser: &mut Parser) -> Result<Expr, Error> {
    /* For now we only support anonymous function */
    /* FIXME we need to do a savepoint of the parser */
    //parse_anon_fun(parser)
        //.map(|anon_fun| Expr::AnonFun(anon_fun))
        //.or_else(|_| parse_fun_call(parser)
                        //.map(|fun_call| Expr::FunCall(fun_call)))
    return
        fallback(parser,
            parse_anon_fun_expr,
            parse_fun_call_expr);

    fn parse_anon_fun_expr(parser: &mut Parser) -> Result<Expr, Error> {
        parse_anon_fun(parser)
            .map(|anon_fun| Expr::AnonFun(anon_fun))
    }

    fn parse_fun_call_expr(parser: &mut Parser) -> Result<Expr, Error> {
        parse_fun_call(parser)
            .map(|fun_call| Expr::FunCall(fun_call))
    }
}

fn fallback<F1, F2, T>(parser: &mut Parser, parse_fun1: F1, parse_fun2: F2)
    -> Result<T, Error>
    where
        F1: FnOnce(&mut Parser) -> Result<T, Error>,
        F2: FnOnce(&mut Parser) -> Result<T, Error>,
{
    fn pick_furthest(err1: Error, err2: Error) -> Error {
        use std::cmp::Ordering::*;
        let ord = unsafe {
            errpos::compare(&err1.pos, &err2.pos)
        };
        match ord {
            Greater | Equal => err1,
            Less            => err2,
        }
    }

    let savepoint = parser.clone();

    parse_fun1(parser)
        .or_else(|prev_err| {
            *parser = savepoint;
            parse_fun2(parser)
                .map_err(|new_err| pick_furthest(prev_err, new_err))
        })
}

// AnonFun

type TypeParamF = Local;
type ValParam = Local;

#[derive(Debug)]
struct AnonFun {
    type_params: Vec<TypeParamF>,
    val_params:  Vec<ValParam>,
    body:        Box<Expr>  /* avoid recursive type */
}

fn parse_anon_fun(parser: &mut Parser) -> Result<AnonFun, Error> {
    match_keyword(parser, "\\")?;
    let type_params = repeat_match(parser, parse_fun_type_param);
    /* TODO parse regular arguments */
    match_keyword(parser, "->")?;
    let body = parse_expression(parser)?;
    Ok(AnonFun {
        type_params,
        val_params: Vec::new(),
        body:     Box::new(body)
    })
}

fn repeat_match<T, E, F>(parser: &mut Parser, parse_once: F) -> Vec<T>
    where
        F: Fn(&mut Parser) -> Result<T, E>
{
    let mut all_matches = Vec::new();
    while let Ok(new_match) = parse_once(parser) {
        all_matches.push(new_match);
    }
    return all_matches;
}

type ArgName = Local;

fn parse_fun_type_param(parser: &mut Parser) -> Result<ArgName, Error> {
    parser.open('(')?;
    match_keyword(parser, "@")?;
    let arg_name = parse_local(parser)?;
    parser.close(')')?;
    Ok(arg_name)
}

// FunCall

type TypeArg = Local;
type ValArg = Local;

#[derive(Debug)]
struct FunCall {
    called_fun: Global,
    type_args:  Vec<TypeArg>,
    val_args:   Vec<ValArg>
}

fn parse_fun_call(parser: &mut Parser) -> Result<FunCall, Error> {
    let called_fun = parse_global(parser)?;
    /* TODO parse the arguments */
    Ok(FunCall {
        called_fun,
        type_args: Vec::new(),
        val_args:  Vec::new()
    })
}

// Local

#[derive(Debug)]
struct Local (String);

fn parse_local(parser: &mut Parser) -> Result<Local, Error> {
    lazy_static! {
        static ref LOCAL_NAME_RE: Regex =
            Regex::new(r"^[a-zA-Z][a-zA-Z0-9_]*")
                .unwrap();
    }
    match parser.match_re(&LOCAL_NAME_RE) {
        Some(mtch) =>
            Ok(Local(
                String::from(mtch))),
        None =>
            parser_err(parser, Reason::LocalNotFound)
    }
}

// Global

#[derive(Debug, Hash, Eq, Ord, PartialOrd, PartialEq, Clone)]
pub struct Global (String);

fn parse_global(parser: &mut Parser) -> Result<Global, Error> {
    /* FIXED: the next char after the name must be a space (or end of line)
     * Otherwise we get:
     * ExpectedKeyword("="):
     * GHC.Base.
     *         ^
     * The Rust Regex library does not support "lookaheads".
     * Instead, add a '\s' at the end of the regex and capture the rest.
     */
    lazy_static! {
        static ref GLOBAL_RE: Regex =
            Regex::new(r"^((?:[a-zA-Z][a-zA-Z0-9_]*\.)*[a-zA-Z][a-zA-Z0-9_]*)\s")
                .unwrap();
    }
    match parser.match_re_captures(&GLOBAL_RE) {
        Some(groups) => {
            let mtch = groups.get(1).unwrap();
            Ok(Global(
                String::from(mtch.as_str())))
        },
        None =>
            parser_err(parser, Reason::GlobalNotFound)
    }
}

impl fmt::Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Into<String> for Global {
    fn into(self) -> String {
        self.0
    }
}

// Error convertion

fn parser_err<T>(parser: &mut Parser, reason: Reason) -> Result<T, Error> {
    Err(
        parser.err(
            super::Reason::Fun(reason)))
}

fn format_parsing_error(err: &Error, original_input: &str, file_name: &path::Path)
    -> Result<String, String>
{
    let fatal_err = ||
        format!("Fatal error: couldn't generate a report for {:?} in {}",
            err.reason,
            file_name.display());

    let line_number = unsafe {
        errpos::line_number(&err.pos, original_input)
            .ok_or_else(fatal_err)?
    };

    let input_marking =
        errpos::report(&err.pos, original_input)
            .map_err(|_| fatal_err())?;

    let final_report =
        format!("{}, line {}:\n{:?}\n{}",
            file_name.display(),
            line_number,
            err.reason,
            input_marking);

    Ok(final_report)
}

fn ignore_decl_error(err: &Error, original_input: &str) -> bool {
    /* Same as before: we know how to handle GlobalNotFound, but nothing else */
    use super::Reason as GenReason;
    let safely_ignore = true;
    let dont_ignore = false;
    let err_pos = &err.pos;

    return match &err.reason {
        /* GlobalNotFound is the first possible error when parsing a declaration.
         * If it's at the beginning of the line, we just couldn't parse the symbol
         * in the declaration. This is usually a statement we don't support (e.g. comments).
         */
        GenReason::Fun(Reason::GlobalNotFound)
            if is_at_beginning_of_line(err_pos, original_input)
            => safely_ignore,

        /* ignore 'Result size of CorePrep' */
        GenReason::ExpectedKeyword(keyword)
            if keyword == "="
                && is_result_size_of_core_prep(err_pos, original_input)
            => safely_ignore,

        /* ignore type signatures
         * ex: 'Utils.Prelude.ignore :: forall a. a -> ()'
         */
        GenReason::ExpectedKeyword(keyword)
            if keyword == "="
                && is_signature_decl(err_pos, original_input)
            => safely_ignore,

        /* Any other case */
        _  => dont_ignore,
    };

    fn is_at_beginning_of_line(err_pos: &ErrPos, input: &str) -> bool {
        unsafe {
            errpos::is_at_beginning_of_line(err_pos, input)
                .unwrap_or(false)
        }
    }

    fn err_str_starts_with(err_pos: &ErrPos, input: &str, prefix: &str) -> bool {
        unsafe {
            match errpos::retrieve_slice(err_pos, input) {
                Ok(s)  => s.starts_with(prefix),
                Err(_) => false,
            }
        }
    }

    fn is_result_size_of_core_prep(err_pos: &ErrPos, input: &str) -> bool {
        err_str_starts_with(err_pos, input, "size of CorePrep")
    }

    fn is_signature_decl(err_pos: &ErrPos, input: &str) -> bool {
        err_str_starts_with(err_pos, input, ":: ")
    }
}
