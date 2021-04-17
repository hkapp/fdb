use std::fs;
use std::io;
use regex::Regex;
use lazy_static::lazy_static;

use super::files::DumpFile;

mod typ;
mod fun;
mod errpos;

pub type ParseResult = (typ::TypInfo, fun::Prod);

#[derive(Debug)]
pub enum Err {
    Fun(fun::Err),
    /* Parenthesis errors */
    ExpectedOpeningParenthesis(char),
    ExpectedClosingParenthesis(char),
    NoMatchingOpeningParenthesis(char),
    ParenthesisMismatch(char, char),
    ParenthesisStackNotEmpty(ParensStack),
    /* File / content errors */
    Io(io::Error),
    ReachedEndOfFile
}

pub fn parse_all<I: Iterator<Item = DumpFile>>(dump_files: I) -> Result<ParseResult, Err> {
    let mut typ_ctx = typ::TypInfo::default();
    let mut parsed_funs = Vec::new();

    for ghc_dump in dump_files {
        match ghc_dump {
            DumpFile::TypDump(file_path) => {
                typ::parse(&mut typ_ctx, &file_path)
            },

            DumpFile::FunDump(file_path) => {
                let file_content = fs::read_to_string(&file_path)?;
                let parsed = fun::parse(&file_content)?;
                parsed_funs.push(parsed);
            }
        }
    }

    let final_funs = fun::merge(parsed_funs)?;
    Ok((typ_ctx, final_funs))
}

// Parser helpers

#[allow(dead_code)]
struct Parsed<'a, T> {
    value:    T,
    leftover: &'a str
}

trait Parse
    where Self: std::marker::Sized
{
    type Err;
    fn parse<'a>(input: &'a str) -> Result<Parsed<'a, Self>, Self::Err>;
}

struct Parser<'a > {
    rem_input:    &'a str,     /* remaining input */
    parens_stack: ParensStack  /* stack of open parentheses */
}

pub type ParensStack = Vec<char>;

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser {
            rem_input:    input,
            parens_stack: Vec::new()
        }
    }

    fn has_input_left(&self) -> bool {
        self.rem_input.len() > 0
    }

    fn finalize<T>(self, res: T) -> Result<T, Err> {
        if !self.parens_stack.is_empty() {
            Err(
                Err::ParenthesisStackNotEmpty(self.parens_stack))
        }
        else {
            Ok(res)
        }
    }

    #[allow(dead_code)]
    fn next_with<F, T, E>(&mut self, parse_fun: F) -> Result<T, E>
        where F: FnOnce(&'a str) -> Result<Parsed<'a, T>, E>,
              T: Sized
    {
        /* We just assume that the caller expects no spaces before their function is called */
        self.skip_spaces();
        match parse_fun(self.rem_input) {
            Ok(parsed) => {
                self.rem_input = parsed.leftover;
                Ok(parsed.value)
            }
            Err(err) => {
                Err(err)
            }
        }
    }

    #[allow(dead_code)]
    fn next<T, E>(&mut self) -> Result<T, E>
        where T: Parse<Err = E>,
              E: Into<Err>
    {
        self.next_with(T::parse)
    }

    /* internal only normally */
    fn advance(&mut self, n_chars: usize) {
        self.rem_input = &self.rem_input[n_chars..];
    }

    fn match_re(&mut self, re: &Regex) -> Option<&'a str> {
        self.skip_spaces();
        match re.find(self.rem_input) {
            Some(mtch) => {
                if mtch.start() != 0 {
                    None
                }
                else {
                    self.advance(mtch.end());
                    Some(mtch.as_str())
                }
            },
            None => {
                None
            }
        }
    }

    fn match_keyword(&mut self, keyword: &str) -> Option<()> {
        self.skip_spaces();

        if !self.rem_input.starts_with(keyword) {
            return None;
        }

        /* The next char must be a space, otherwise the keyword did not
         * actually match ('::>' vs. '::' for example)
         */
        match is_space_at(self.rem_input, keyword.len()) {
            Some(true)  => {},           /* is a space: ok */
            Some(false) => return None,  /* not a space: not ok */
            None        => {},           /* end of string: still fine */
        }

        /* '+1' is still safe even if we reached the end of the string */
        self.advance(keyword.len()+1);
        Some(())
    }

    fn skip_spaces(&mut self) {
        self.rem_input = self.rem_input.trim_start()
    }

    fn skip_curr_line(&mut self) {
        /* FIXME this does not support Windows and Mac formats (\r) */
        lazy_static! {
            static ref SKIP_LINE_RE: Regex = Regex::new(r"^[^\n]*\n").unwrap();
        }
        let mtch = self.match_re(&SKIP_LINE_RE);
        if mtch.is_none() {
            /* There was no end of line character.
             * We must be at the end the string.
             */
            self.rem_input = "";
        }
    }

    fn open(&mut self, opening_parens: char) -> Result<(), Err> {
        self.skip_spaces();
        match str_first(self.rem_input) {
            Some(c) if c == opening_parens => {
                self.parens_stack.push(opening_parens);
                self.advance(1);
                Ok(())
            },

            Some(_) =>
                Err(Err::ExpectedOpeningParenthesis(opening_parens)),

            None =>
                Err(Err::ReachedEndOfFile)
        }
    }

    fn close(&mut self, closing_parens: char) -> Result<(), Err> {
        self.skip_spaces();

        match str_first(self.rem_input) {
            Some(c) if c == closing_parens => {
                self.advance(1);
            },

            Some(_) =>
                return Err(Err::ExpectedClosingParenthesis(closing_parens)),

            None =>
                return Err(Err::ReachedEndOfFile)
        };

        let opening_parens = match self.parens_stack.pop() {
            Some(top_parens) =>
                top_parens,

            None =>
                return Err(Err::NoMatchingOpeningParenthesis(closing_parens))
        };

        match (opening_parens, closing_parens) {
            ('(', ')') => Ok(()),
            ('{', '}') => Ok(()),
            ('[', ']') => Ok(()),

            _  =>
                Err(Err::ParenthesisMismatch(opening_parens, closing_parens))
        }
    }
}

fn str_first(s: &str) -> Option<char> {
    s.chars().next()
}

/* Returns None if there are not enough chars in the slice */
fn is_space_at(s: &str, pos: usize) -> Option<bool> {
    s.chars()
        .skip(pos)  /* pos = 0 => skip(0) */
        .next()
        .map(|c| c.is_whitespace())
}

// Error conversion

impl From<io::Error> for Err {
    fn from(err: io::Error) -> Self {
        Err::Io(err)
    }
}

impl From<fun::Err> for Err {
    fn from(err: fun::Err) -> Self {
        Err::Fun(err)
    }
}
