use regex::Regex;

use super::Parser;

pub type Prod = Symbol;

pub fn parse(input: &str) -> Result<Prod, Err> {
    let mut parser = Parser::new(input);
    parse_symbol(&mut parser)
    //println!("Skipping {} chars...", input.len());
    //Ok (())
}

pub fn merge(mut file_prods: Vec<Prod>) -> Result<Prod, Err> {
    file_prods.pop()
        .ok_or(Err::SymbolNotFound)
}

pub struct Symbol (String);

fn parse_symbol(parser: &mut Parser) -> Result<Symbol, Err> {
    /* TODO use lazy_something */
    let re = Regex::new(r"\w+")?;
    match parser.match_re(re) {
        Some(mtch) =>
            Ok(Symbol(
                String::from(mtch))),
        None =>
            Err(Err::SymbolNotFound)
    }
}

// Errors

#[derive(Debug)]
pub enum Err {
    SymbolNotFound,
    RegexError(regex::Error)
}

impl From<regex::Error> for Err {
    fn from(err: regex::Error) -> Self {
        Err::RegexError(err)
    }
}
