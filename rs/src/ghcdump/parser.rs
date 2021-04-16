use std::fs;
use std::io;

mod typ;
mod fun;

use super::files::DumpFile;

pub type ParseResult = (typ::TypInfo, fun::FunInfo);

#[derive(Debug)]
pub enum Err {
    Io(io::Error),
    Fun(fun::Err)
}

pub fn parse_all<I: Iterator<Item = DumpFile>>(dump_files: I) -> Result<ParseResult, Err> {
    let mut typ_ctx = typ::TypInfo::default();
    let mut fun_ctx = fun::FunInfo::default();

    for ghc_dump in dump_files {
        match ghc_dump {
            DumpFile::TypDump(file_path) =>
                typ::parse(&mut typ_ctx, &file_path),

            DumpFile::FunDump(file_path) => {
                let file_content = fs::read_to_string(&file_path)?;
                fun::parse(&mut fun_ctx, &file_content)?
            }
        }
    }

    Ok((typ_ctx, fun_ctx))
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

    fn skip_spaces(&mut self) {
        self.0 = self.0.trim_start()
    }
}

//fn skip_regex(input: &str, re: Regex) -> Result<&str, Err> {
    //match re.find(input) {
        //Some(mtch) => input + mtch.end()
        //None => input
    //}
//}

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
