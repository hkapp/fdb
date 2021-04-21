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
pub struct Error {
    pos:    ErrPos,
    reason: Reason
}

type ErrPos = errpos::ErrPos;

#[derive(Debug)]
pub enum Reason {
    /* Classic parsing errors */
    ExpectedKeyword(String),
    /* Parenthesis errors */
    ExpectedOpeningParenthesis(char),
    ExpectedClosingParenthesis(char),
    NoMatchingOpeningParenthesis(char),
    ParenthesesMismatch(char, char),
    ParenthesisStackNotEmpty(ParensStack),
    /* More specific parsing errors */
    Fun(fun::Reason),
    /* File / content errors */
    Io(io::Error),
    ReachedEndOfFile
}

pub fn parse_all<I: Iterator<Item = DumpFile>>(dump_files: I) -> Result<ParseResult, Error> {
    let mut typ_ctx = typ::TypInfo::default();
    let mut parsed_funs = Vec::new();

    for ghc_dump in dump_files {
        match ghc_dump {
            DumpFile::TypDump(file_path) => {
                typ::parse(&mut typ_ctx, &file_path)
            },

            DumpFile::FunDump(file_path) => {
                let file_content = fs::read_to_string(&file_path)
                                    .map_err(convert_io_err)?;
                let parsed = fun::parse(&file_content, file_path.as_path())?;
                parsed_funs.push(parsed);
            }
        }
    }

    let final_funs = fun::merge(parsed_funs);
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
    type Error;
    fn parse<'a>(input: &'a str) -> Result<Parsed<'a, Self>, Self::Error>;
}

#[derive(Clone)]
struct Parser<'a > {
    rem_input:    &'a str,     /* remaining input */
    parens_stack: ParensStack  /* stack of open parentheses */
}

pub type ParensStack = Vec<(char, ErrPos)>;

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

    fn finalize<T>(mut self, res: T) -> Result<T, Error> {
        if !self.parens_stack.is_empty() {
            let parens_stack = self.parens_stack;
            self.parens_stack = Vec::new();  /* move occurs above */

            let first_parens = parens_stack.first().unwrap();
            let first_parens_pos = first_parens.1.clone();

            let reason = Reason::ParenthesisStackNotEmpty(parens_stack);
            let err = self.err_since(reason, first_parens_pos);

            Err(err)
        }
        else {
            Ok(res)
        }
    }

    fn err(&self, reason: Reason) -> Error {
        Error {
            pos: ErrPos::at(self.rem_input),
            reason
        }
    }

    fn err_since(&self, reason: Reason, prev_pos: ErrPos) -> Error {
        let pos = ErrPos::between(prev_pos, self.rem_input).unwrap();
        Error {
            pos,
            reason
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
        where T: Parse<Error = E>,
              E: Into<Error>
    {
        self.next_with(T::parse)
    }

    /* internal only normally */
    fn advance(&mut self, n_chars: usize) {
        self.rem_input = &self.rem_input[n_chars..];
    }

    /* should be internal */
    fn advance_match(&mut self, mtch: regex::Match<'a>)
        -> Option<regex::Match<'a>>
    {
        if mtch.start() != 0 {
            None
        }
        else {
            self.advance(mtch.end());
            Some(mtch)
        }
    }

    fn match_re(&mut self, re: &Regex) -> Option<&'a str> {
        self.skip_spaces();
        match re.find(self.rem_input) {
            Some(mtch) => {
                self.advance_match(mtch)
                    .map(|m| m.as_str())
            },
            None => {
                None
            }
        }
    }

    fn match_re_captures(&mut self, re: &Regex)
        -> Option<regex::Captures<'a>>
    {
        self.skip_spaces();
        match re.captures(self.rem_input) {
            Some(mtch) => {
                let whole_match = mtch.get(0).unwrap();
                self.advance_match(whole_match)
                    .map(|_| mtch)
            },
            None => {
                None
            }
        }
    }

    fn match_keyword(&mut self, keyword: &str) -> Result<(), Error> {
        /* acts as a lazy value (can't use a closure because it
         * creates multiple mutable references)
         */
        fn gen_err(parser: &mut Parser, keyword: &str) -> Result<(), Error> {
            Err(parser.err(
                Reason::ExpectedKeyword(
                    String::from(keyword))))
        }

        self.skip_spaces();

        if !self.rem_input.starts_with(keyword) {
            return gen_err(self, keyword);
        }

        /* The next char must be a space, otherwise the keyword did not
         * actually match ('::>' vs. '::' for example)
         */
        match is_space_at(self.rem_input, keyword.len()) {
            Some(true)  => {},  /* is a space: ok */
            Some(false) =>      /* not a space: not ok */
                return gen_err(self, keyword),
            None        => {},  /* end of string: still fine */
        }

        /* '+1' is still safe even if we reached the end of the string */
        self.advance(keyword.len()+1);
        Ok(())
    }

    fn skip_spaces(&mut self) {
        self.rem_input = self.rem_input.trim_start()
    }

    #[allow(dead_code)]
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

    fn open(&mut self, opening_parens: char) -> Result<(), Error> {
        self.skip_spaces();
        match str_first(self.rem_input) {
            Some(c) if c == opening_parens => {
                let save = (opening_parens, ErrPos::at(self.rem_input));
                self.parens_stack.push(save);
                self.advance(1);
                Ok(())
            },

            Some(_) =>
                Err(self.err(
                    Reason::ExpectedOpeningParenthesis(opening_parens))),

            None =>
                Err(self.err(
                    Reason::ReachedEndOfFile))
        }
    }

    fn close(&mut self, closing_parens: char) -> Result<(), Error> {
        self.skip_spaces();

        match str_first(self.rem_input) {
            Some(c) if c == closing_parens => {
                self.advance(1);
            },

            Some(_) =>
                return Err(self.err(
                    Reason::ExpectedClosingParenthesis(closing_parens))),

            None =>
                return Err(self.err(
                    Reason::ReachedEndOfFile))
        };

        let (opening_parens, opening_pos) =
            match self.parens_stack.pop() {
                Some(top_parens) =>
                    top_parens,

                None =>
                    return Err(self.err(
                        Reason::NoMatchingOpeningParenthesis(closing_parens)))
            };

        match (opening_parens, closing_parens) {
            ('(', ')') => Ok(()),
            ('{', '}') => Ok(()),
            ('[', ']') => Ok(()),

            _  =>
                Err(self.err_since(
                        Reason::ParenthesesMismatch(opening_parens, closing_parens),
                        opening_pos))
        }
    }

    #[allow(dead_code)]
    fn control<F>(&mut self, arbitrary_fun: F)
        where
            F: FnOnce(&str) -> usize
    {
        let skip_n = arbitrary_fun(self.rem_input);
        if skip_n > 0 {
            self.advance(skip_n);
            println!("after control: {}", &self.rem_input[0..20]);
        }
    }

    fn peek<F, T>(&mut self, arbitrary_fun: F) -> T
        where
            F: FnOnce(&str) -> T
    {
        arbitrary_fun(self.rem_input)
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

fn convert_io_err(err: io::Error) -> Error {
    let pos = unsafe {
        ErrPos::none()
    };
    let reason = Reason::Io(err);

    Error {
        pos,
        reason
    }
}
