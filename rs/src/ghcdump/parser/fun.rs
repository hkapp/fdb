use regex::Regex;
use lazy_static::lazy_static;

use super::Parser;

// Export

pub type Prod = Vec<Decl>;

pub fn parse(input: &str) -> Result<Prod, super::Err> {
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

pub fn merge(file_prods: Vec<Prod>) -> Result<Prod, Err> {
    let mut final_res = Vec::new();
    for mut decls in file_prods.into_iter() {
        final_res.append(&mut decls);
    }
    Ok(final_res)
}

// Decl

#[derive(Debug)]
pub struct Decl {
    name: Global,
    body: Expr
}

fn parse_decl(parser: &mut Parser) -> Result<Decl, super::Err> {
    let fun_name = parse_global(parser)?;
    match_keyword(parser, "=")?;
    let body = parse_expression(parser)?;
    Ok(Decl {
        name: fun_name,
        body: body
    })
}

fn match_keyword(parser: &mut Parser, keyword: &str) -> Result<(), Err> {
    parser.match_keyword(keyword)
        .ok_or_else(|| Err::ExpectedKeyword(String::from(keyword)))
}

// Expr

/* TODO consider type Expr = Box<Expr_> */
#[derive(Debug)]
enum Expr {
    AnonFun(AnonFun),
    Nothing /* TODO remove */
}

fn parse_expression(parser: &mut Parser) -> Result<Expr, Err> {
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

fn parse_anon_fun(parser: &mut Parser) -> Result<AnonFun, Err> {
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

fn parse_polytyp_argument(parser: &mut Parser) -> Result<ArgName, super::Err> {
    parser.open('(')?;
    match_keyword(parser, "@")?;
    let arg_name = parse_local(parser)?;
    parser.close(')')?;
    Ok(arg_name)
}

// Local

#[derive(Debug)]
struct Local (String);

fn parse_local(parser: &mut Parser) -> Result<Local, Err> {
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
            Err(Err::LocalNotFound)
    }
}

// Global

#[derive(Debug)]
pub struct Global (String);

fn parse_global(parser: &mut Parser) -> Result<Global, Err> {
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
            Err(Err::SymbolNotFound)
    }
}

// Errors

#[derive(Debug)]
pub enum Err {
    SymbolNotFound,
    LocalNotFound,
    RegexError(regex::Error), /* TODO remove? */
    ExpectedKeyword(String)
}

impl From<regex::Error> for Err {
    fn from(err: regex::Error) -> Self {
        Err::RegexError(err)
    }
}

fn try_handling_err(err: super::Err) -> Result<Option<String>, super::Err> {
    /* Same as before: we know how to handle SymbolNotFound, but nothing else */
    match &err {
        super::Err::Fun(local_err) =>
            match local_err {
                Err::SymbolNotFound => Ok(None),
                _                   => Err(err),
            }
        _                        => Err(err),
    }
}
