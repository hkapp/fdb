use regex::Regex;
use lazy_static::lazy_static;

use super::Parser;

type Error = super::Error;

#[derive(Debug)]
pub enum Reason {
    GlobalNotFound,
    LocalNotFound,
    RegexError(regex::Error), /* TODO remove? */
}

// Export

pub type Prod = Vec<Decl>;

pub fn parse(input: &str) -> Result<Prod, Error> {
    let mut parser = Parser::new(input);
    let mut declarations = Vec::new();

    while parser.has_input_left() {
        match parse_decl(&mut parser) {
            Ok(decl) => {
                println!("{:?} ", decl);
                declarations.push(decl);
            }
            Err(err) => {
                match try_handling_err(err) {
                    Ok(None) => {
                        /* no report, just skip it */
                        parser.skip_curr_line();
                    },

                    Ok(Some(report)) =>  {
                        /* error report generated, print it */
                        println!("{}", report);
                        parser.skip_curr_line();
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
    Nothing /* TODO remove */
}

fn parse_expression(parser: &mut Parser) -> Result<Expr, Error> {
    /* For now we only support anonymous function */
    parse_anon_fun(parser)
        .map(|anon_fun| Expr::AnonFun(anon_fun))
}

// AnonFun

type FTypArg = Local;

#[derive(Debug)]
struct AnonFun {
    polytyp_args: Vec<FTypArg>,
    val_args:     Vec<Local>,
    body:         Box<Expr>  /* avoid recursive type */
}

fn parse_anon_fun(parser: &mut Parser) -> Result<AnonFun, Error> {
    match_keyword(parser, "\\")?;
    let polytyp_args = repeat_match(parser, parse_polytyp_argument);
    Ok(AnonFun {
        polytyp_args,
        val_args: Vec::new(),
        body:     Box::new(Expr::Nothing)
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

fn parse_polytyp_argument(parser: &mut Parser) -> Result<ArgName, Error> {
    parser.open('(')?;
    match_keyword(parser, "@")?;
    let arg_name = parse_local(parser)?;
    parser.close(')')?;
    Ok(arg_name)
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
    lazy_static! {
        static ref GLOBAL_RE: Regex =
            Regex::new(r"^([a-zA-Z][a-zA-Z0-9_]*\.)*[a-zA-Z][a-zA-Z0-9_]*")
                .unwrap();
    }
    match parser.match_re(&GLOBAL_RE) {
        Some(mtch) =>
            Ok(Global(
                String::from(mtch))),
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

fn try_handling_err(err: Error) -> Result<Option<String>, Error> {
    /* Same as before: we know how to handle GlobalNotFound, but nothing else */
    match &err.reason {
        super::Reason::Fun(
            Reason::GlobalNotFound) => Ok(None),
        _                           => Err(err),
    }
}
