mod files;
mod parser;
mod ir;
mod convir;

use std::path::Path;
use std::io;

use crate::ir as cmnir;

#[derive(Debug)]
pub enum FatalError {
    IoError(io::Error)
}

type GhcError = String; // FIXME
type ParsedDecl = (cmnir::Global, Result<cmnir::Decl, GhcError>);

pub type ParseError = String;

const GHC_DUMP_DIR: &str = "../hs/bin/src";

pub fn load_all() -> Result<impl Iterator<Item=ParsedDecl>, FatalError> {
    load_everything_under(&Path::new(GHC_DUMP_DIR))
}

fn load_everything_under(root_dir: &Path) -> Result<impl Iterator<Item=ParsedDecl>, FatalError> {
    let files_to_parse = files::find_dump_files(root_dir);
    let ghc_decls = parser::parse_all(files_to_parse)?;
    let conv_decls = convert_all_decls(ghc_decls);
    Ok(conv_decls)
}

fn convert_all_decls(ghc_decls: parser::ParseResult) -> impl Iterator<Item=ParsedDecl> {
    /* parser::ParseResult = HashMap<Global, Result<Decl, String>>; */
    ghc_decls.into_iter()
        .map(|(name, decl_or_err)| {
            let conv_decl_or_err: Result<cmnir::Decl, GhcError> =
                decl_or_err
                    .and_then(|decl|
                        convir::conv_decl(&decl)
                            .map_err(|conv_err| format!("{:?}", conv_err))
                    );
            let conv_name = convir::conv_global(name);
            (conv_name, conv_decl_or_err)
        })
}

// Error conversion

fn convert_io_err(err: io::Error) -> FatalError {
    FatalError::IoError(err)
}
