use std::path::Path;
use std::io;

mod files;
mod parser;
pub mod ir;

#[derive(Debug)]
pub enum FatalError {
    IoError(io::Error)
}

pub type ParseError = String;

const GHC_DUMP_DIR: &str = "../hs/bin/src";

pub fn load_all() -> Result<parser::ParseResult, FatalError> {
    load_everything_under(&Path::new(GHC_DUMP_DIR))
}

fn load_everything_under(root_dir: &Path) -> Result<parser::ParseResult, FatalError> {
    parser::parse_all(
        files::find_dump_files(root_dir))
}

// Error conversion

fn convert_io_err(err: io::Error) -> FatalError {
    FatalError::IoError(err)
}
