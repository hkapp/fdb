use std::fs;
use std::io;
use regex::Regex;

use super::files::DumpFile;

mod typ;
mod fun;

pub type ParseResult = (typ::TypInfo, fun::Prod);

#[derive(Debug)]
pub enum Err {
    Io(io::Error),
    Fun(fun::Err)
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

struct Parser<'a > (&'a str);

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser(input)
    }

    fn has_input_left(&self) -> bool {
        self.0.len() > 0
    }

    fn into_parsed<T>(self, res: T) -> Parsed<'a, T> {
        Parsed {
            value:    res,
            leftover: self.0
        }
    }

    fn next_with<F, T, E>(&mut self, parse_fun: F) -> Result<T, E>
        where F: FnOnce(&'a str) -> Result<Parsed<'a, T>, E>,
              T: Sized
    {
        /* We just assume that the caller expects no spaces before their function is called */
        self.skip_spaces();
        match parse_fun(self.0) {
            Ok(parsed) => {
                self.0 = parsed.leftover;
                Ok(parsed.value)
            }
            Err(err) => {
                Err(err)
            }
        }
    }

    fn next<T, E>(&mut self) -> Result<T, E>
        where T: Parse<Err = E>,
              E: Into<Err>
    {
        self.next_with(T::parse)
    }

    fn match_re(&mut self, re: Regex) -> Option<&'a str> {
        match re.find(self.0) {
            Some(mtch) => {
                if mtch.start() != 0 {
                    None
                }
                else {
                    self.0 = &self.0[mtch.end()..];
                    Some(mtch.as_str())
                }
            },
            None => {
                None
            }
        }
    }

    fn skip_spaces(&mut self) {
        self.0 = self.0.trim_start()
    }

    fn skip_curr_line(&mut self) {
        /* TODO use lazy_static */
        /* FIXME this does not support Windows and Mac formats (\r) */
        let re = Regex::new(r"^[^\n]*\n").unwrap();
        let mtch = self.match_re(re);
        if mtch.is_none() {
            /* There was no end of line character.
             * We must be at the end the string.
             */
            self.0 = "";
        }
    }
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
