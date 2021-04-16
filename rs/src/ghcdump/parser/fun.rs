use regex::Regex;
use lazy_static::lazy_static;

use super::Parser;

pub type Prod = Vec<Symbol>;

pub fn parse(input: &str) -> Result<Prod, Err> {
    let mut parser = Parser::new(input);
    let mut symbols = Vec::new();
    while parser.has_input_left() {
        match parse_symbol(&mut parser) {
            Ok(sym) => {
                print!("{} ", sym.0);
                symbols.push(sym);
            }
            Err(Err::SymbolNotFound) => {
                /* Just skip the line and try again */
                parser.skip_curr_line();
            }
            Err(other_err) => {
                return Err(other_err);
            }
        }
    }

    Ok(symbols)
}

pub fn merge(mut file_prods: Vec<Prod>) -> Result<Prod, Err> {
    file_prods.pop()
        .ok_or(Err::SymbolNotFound)
}

pub struct Symbol (String);

fn parse_symbol(parser: &mut Parser) -> Result<Symbol, Err> {
    lazy_static! {
        static ref SYMBOL_RE: Regex =
            Regex::new(r"^([a-zA-Z][a-zA-Z0-9_]*\.)*[a-zA-Z][a-zA-Z0-9_]*")
                .unwrap();
    }
    match parser.match_re(&SYMBOL_RE) {
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
