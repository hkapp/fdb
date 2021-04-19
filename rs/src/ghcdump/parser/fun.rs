use regex::Regex;
use lazy_static::lazy_static;
use std::path;

use super::{Parser, ErrPos};
use super::errpos;

type Error = super::Error;

#[derive(Debug)]
pub enum Reason {
    GlobalNotFound,
    LocalNotFound,
}

// Export

pub type Prod = Vec<Decl>;

pub fn parse(input: &str, file_name: &path::Path) -> Result<Prod, Error> {
    let mut parser = Parser::new(input);
    let mut declarations = Vec::new();

    while parser.has_input_left() {
        match parse_decl(&mut parser) {
            Ok(decl) => {
                println!("{:?} ", decl);
                declarations.push(decl);
            }
            Err(err) => {
                match try_handling_err(&err, input) {
                    Ok(None) => {
                        /* no report, just skip it */
                        skip_after_empty_line(&mut parser);
                    },

                    Ok(Some(report)) =>  {
                        /* error report generated, print it */
                        /* TODO also print filename and line number */
                        let line_number = unsafe {
                            errpos::line_number(&err.pos, input).unwrap()
                        };
                        println!("\n{}, line {}:", file_name.display(), line_number);
                        println!("{:?}", err.reason);
                        println!("{}", report);
                        skip_after_empty_line(&mut parser);
                    },

                    Err(fatal_err) => {
                        /* don't know how to handle this, propagate up */
                        return Err(fatal_err)
                    }
                }
            }
        }
    }

    parser.finalize(declarations)
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
    let mut final_res = Vec::new();
    for mut decls in file_prods.into_iter() {
        final_res.append(&mut decls);
    }
    final_res
}

// Decl

#[derive(Debug)]
pub struct Decl {
    name: Global,
    body: Expr
}

fn parse_decl(parser: &mut Parser) -> Result<Decl, Error> {
    let fun_name = parse_global(parser)?;
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
    Nothing /* TODO remove */
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

#[derive(Debug)]
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

// Error convertion

fn parser_err<T>(parser: &mut Parser, reason: Reason) -> Result<T, Error> {
    Err(
        parser.err(
            super::Reason::Fun(reason)))
}

fn try_handling_err(err: &Error, original_input: &str) -> Result<Option<String>, Error> {
    /* Same as before: we know how to handle GlobalNotFound, but nothing else */
    use super::Reason as GenReason;
    let safely_ignore = Ok(None);
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

        _  =>
            Ok(
                errpos::report(err_pos, original_input).ok()),  /* FIXME we're losing the
                                                                 * report error here, if any */
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
