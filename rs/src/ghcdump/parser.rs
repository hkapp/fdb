use std::fs;

mod typ;
mod fun;

use super::files::DumpFile;

pub type ParseResult = (typ::TypInfo, fun::FunInfo);
pub type ParseErr = std::io::Error;

pub fn parse_all<I: Iterator<Item = DumpFile>>(dump_files: I) -> Result<ParseResult, ParseErr> {
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
