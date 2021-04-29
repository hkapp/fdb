use regex::Regex;
use lazy_static::lazy_static;
use std::path;
use std::collections::HashMap;

use super::{Parser, ErrPos};
use super::errpos;
use crate::ghcdump::ir::*;

type Error = super::Error;

#[derive(Debug)]
pub enum Reason {
    GlobalNotFound,
    LocalNotFound,
    IdentifierNotProperlyEnded,
    MismatchedLetNames{ signature_name: Local, body_name: Local },
    MissingGhcAnnotation,
    KeywordNotAnIdentifier(String),
    ExpectedIdentifier,
    ExpectedALiteral,
    InvalidIntLit { int_lit_str: String, parse_int_err: std::num::ParseIntError },
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

fn parse_expression(parser: &mut Parser) -> Result<Expr, Error> {
    /* TODO implement the cascade macro */
    return
        fallback(parser,
            parse_let_expr,
            |prs| fallback(prs,
                parse_anon_fun_expr,
                |prs| fallback(prs,
                    parse_lit_conv_expr,
                    parse_fun_call_expr)));

    fn parse_let_expr(parser: &mut Parser) -> Result<Expr, Error> {
        parse_let(parser)
            .map(|let_expr| Expr::LetExpr(let_expr))
    }

    fn parse_anon_fun_expr(parser: &mut Parser) -> Result<Expr, Error> {
        parse_anon_fun(parser)
            .map(|anon_fun| Expr::AnonFun(anon_fun))
    }

    fn parse_lit_conv_expr(parser: &mut Parser) -> Result<Expr, Error> {
        parse_lit_conv(parser)
            .map(|lit_conv| Expr::LitConv(lit_conv))
    }

    fn parse_fun_call_expr(parser: &mut Parser) -> Result<Expr, Error> {
        parse_fun_call(parser)
            .map(|fun_call| Expr::FunCall(fun_call))
    }
}

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

fn fallback<F1, F2, T>(parser: &mut Parser, parse_fun1: F1, parse_fun2: F2)
    -> Result<T, Error>
    where
        F1: FnOnce(&mut Parser) -> Result<T, Error>,
        F2: FnOnce(&mut Parser) -> Result<T, Error>,
{
    let savepoint = parser.savepoint();

    parse_fun1(parser)
        .or_else(|prev_err| {
            parser.restore(savepoint);
            parse_fun2(parser)
                .map_err(|new_err| pick_furthest(prev_err, new_err))
        })
}

/* TODO cascade macro */

/* LetExpr */

fn parse_let(parser: &mut Parser) -> Result<LetExpr, Error> {
    /* Example: */
    /* let {
         sat_segB [Occ=Once] :: GHC.Integer.Type.Integer
         [LclId]
         sat_segB = %expr } in
       %expr
     */

    match_keyword(parser, "let")?;
    parser.open('{')?;

    let var_name = parse_local(parser)?;
    ignore_decl_annot(parser)?;
    match_keyword(parser, "::")?;
    let var_type = parse_type(parser)?;

    ignore_decl_annot(parser)?;
    let var_name2 = parse_local(parser)?;

    if var_name2 != var_name {
        /* Some weird parsing scenario where we parsed another
         * declaration instead of the body declaration?
         */
        return parser_err(parser,
                    Reason::MismatchedLetNames {
                        signature_name: var_name,
                        body_name:      var_name2
                    });
    }

    match_keyword(parser, "=")?;
    let body = parse_expression(parser)?;

    parser.close('}')?;


    let let_expr =
        LetExpr {
            var_name,
            var_type,
            body: Box::new(body)
        };
    Ok(let_expr)
}

// AnonFun

fn parse_anon_fun(parser: &mut Parser) -> Result<AnonFun, Error> {
    match_keyword(parser, "\\")?;

    let (type_params, tp_err) = repeat_match(parser, parse_fun_type_param);

    let (val_params,  vp_err) = repeat_match(parser, parse_val_param);

    match_keyword(parser, "->")
        .map_err(|arr_err| {
            /* To provide a better error position, pick the furthest
             * error message out of the three previous errors.
             */
            pick_furthest(arr_err,
                pick_furthest(tp_err, vp_err))
        })?;

    let body = parse_expression(parser)?;

    Ok(AnonFun {
        type_params,
        val_params,
        body: Box::new(body)
    })
}

fn repeat_match<T, E, F>(parser: &mut Parser, parse_once: F) -> (Vec<T>, E)
    where
        F: Fn(&mut Parser) -> Result<T, E>
{
    let mut all_matches = Vec::new();
    let mut savepoint = parser.savepoint();
    let mut last_parse = parse_once(parser);

    while let Ok(new_match) = last_parse {
        all_matches.push(new_match);
        savepoint = parser.savepoint();
        last_parse = parse_once(parser);
    }

    /* Last parsing failed, so we need to restore the parser state */
    parser.restore(savepoint);

    let last_err = last_parse.map(|_| ()).unwrap_err();
    (all_matches, last_err)
}

// TypeParamF(unctions)

fn parse_fun_type_param(parser: &mut Parser) -> Result<TypeParamF, Error> {
    parser.open('(')?;
    match_keyword(parser, "@")?;
    let param_name = parse_local(parser)?;
    parser.close(')')?;
    Ok(param_name)
}

// ValParam

fn parse_val_param(parser: &mut Parser) -> Result<ValParam, Error> {
    /* '(x_segA [Occ=Once] :: Main.QVal)' */
    parser.open('(')?;
    let name = parse_local(parser)?;

    /* Ignore the occurrence mark from GHC */
    /* '[Occ=Once]' */
    ignore_decl_annot(parser)?;

    match_keyword(parser, "::")?;

    let param_type = parse_type(parser)?;

    parser.close(')')?;

    let val_param =
        ValParam {
            name,
            typ: param_type
        };
    Ok(val_param)
}

fn ignore_decl_annot(parser: &mut Parser) -> Result<(), Error> {
    /* Ignore square brackets annotations from GHC */
    /* ex: '[Occ=Once]' */
    /* Note: parsing this properly requires adding an option to the Parser,
     * otherwise it expects that there is a space right after the "=", which
     * is not the case here.
     */
    lazy_static! {
        static ref OCC_RE: Regex =
            Regex::new(r"^\[[^\]]*\]")
                .unwrap();
    }

    parser.match_re(&OCC_RE)
        .map(|_| ())
        .ok_or_else(|| parser_error(parser, Reason::MissingGhcAnnotation))
}

// Type

fn parse_type(parser: &mut Parser) -> Result<Type, Error> {
    /* For now we only support non-argumented types */
    parse_global(parser)
}

// LitConv

fn parse_lit_conv(parser: &mut Parser) -> Result<LitConv, Error> {
    /* ex: 'GHC.Types.I# 13#' */
    let conv_fun = parse_lit_conv_fun_name(parser)?;
    let raw_lit = parse_raw_lit(parser)?;

    Ok(LitConv {
        conv_fun,
        raw_lit
    })
}

fn parse_lit_conv_fun_name(parser: &mut Parser) -> Result<Global, Error> {
    /* Basically a Global that ends with '#' */
    lazy_static! {
        static ref LIT_CONV_FUN_RE: Regex = {
            let mut regex_pattern = global_regex_pattern();
            regex_pattern.push_str("#");

            Regex::new(&regex_pattern).unwrap()
        };
    }

    parse_identifier(parser, &LIT_CONV_FUN_RE)
        .map(|s| Global(String::from(s)))
}

fn parse_raw_lit(parser: &mut Parser) -> Result<RawLit, Error> {
    /* TODO also parse string literals */
    lazy_static! {
        static ref RAW_LIT_INT_RE: Regex = {
            Regex::new(r"([0-9]+)#").unwrap()
        };
    }

    let captured = parser.match_re_captures(&RAW_LIT_INT_RE)
                    .ok_or_else(|| parser_error(parser, Reason::ExpectedALiteral))?;

    let int_as_str: &str = captured.get(1).unwrap().as_str();
    let int_lit = int_as_str.parse()
                    .map_err(|parse_int_err|
                        parser_error(parser,
                            Reason::InvalidIntLit {
                                int_lit_str: String::from(int_as_str),
                                parse_int_err
                            }))?;
                    /* FIXME the error position is wrong here */
    let raw_lit = RawLit::IntLit(int_lit);
    Ok(raw_lit)
}

// FunCall

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

    /* TODO validate the identifier */
}

// Global

const IDEN_START_PAT: &str = r"[a-zA-Z]";

const PKG_START_PAT: &str = IDEN_START_PAT;
const PKG_BODY_PAT:  &str = r"[a-zA-Z0-9_]";

const VAR_START_PAT:     &str = IDEN_START_PAT;
const VAR_BODY_PAT:      &str = r"[a-zA-Z0-9_]";

fn global_regex_pattern() -> String {
    format!(r"^(?:{}{}*\.)*{}{}*",
            PKG_START_PAT, PKG_BODY_PAT,
            VAR_START_PAT, VAR_BODY_PAT)
}

fn parse_global(parser: &mut Parser) -> Result<Global, Error> {
    lazy_static! {
        static ref GLOBAL_RE: Regex = {
            let regex_str = global_regex_pattern();
            //Regex::new(r"^(?:[a-zA-Z][a-zA-Z0-9_]*\.)*[a-zA-Z][a-zA-Z0-9_]*")
            Regex::new(&regex_str).unwrap()
        };
    }

    parse_identifier(parser, &GLOBAL_RE)
        .map(|s| Global(String::from(s)))
        .map_err(|_| parser_error(parser, Reason::GlobalNotFound))
}

fn parse_identifier<'a, 'b>(parser: &'a mut Parser, regex: &'b Regex) -> Result<&'a str, Error> {
    let raw_iden = parser.match_re(regex)
                        .ok_or_else(|| parser_error(parser, Reason::ExpectedIdentifier))?;

    validate_identifier(parser, raw_iden)
        .map(|_| raw_iden)
}

const IDENTIFIER_ENDERS: [char; 3] = [' ', '\n', ')'];

fn validate_identifier(parser: &Parser, iden_str: &str) -> Result<(), Error> {
    /* 1. Check if the identifier ends correctly */
    /* FIXED: the next char after the name must be a space (or end of line)
     * Otherwise we get:
     * ExpectedKeyword("="):
     * GHC.Base.
     *         ^
     * The Rust Regex library does not support "lookaheads".
     * Instead, peek right after the basic Regex to see if the
     * next character is a space or ')' and reject anything else.
     */
    let ends_correctly =
        parser.peek(|s|
            (s.len() == 0) || (s.starts_with(&IDENTIFIER_ENDERS[..])));

    if !ends_correctly {
        return parser_err(parser, Reason::IdentifierNotProperlyEnded);
    }

    /* 2. Check if the identifier is a keyword */
    if iden_str == "let" {
        /* FIXME the error position is wrong here */
        return parser_err(parser,
                    Reason::KeywordNotAnIdentifier(
                        String::from(iden_str)));
    }

    Ok(())
}

// Error conversion

fn parser_err<T>(parser: &Parser, reason: Reason) -> Result<T, Error> {
    Err(
        parser_error(parser, reason))
}

fn parser_error(parser: &Parser, reason: Reason) -> Error {
    parser.err(
        super::Reason::Fun(reason))
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
