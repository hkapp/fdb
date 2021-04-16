use std::path::Path;

use super::files::DumpFile;

pub fn parse_all<I: Iterator<Item = DumpFile>>(dump_files: I) {
    let mut typ_ctx = TypParseContext::default();
    let mut fun_ctx = FunParseContext::default();

    for ghc_dump in dump_files {
        match ghc_dump {
            DumpFile::TypDump(file_path) =>
                parse_types(&mut typ_ctx, &file_path),

            DumpFile::FunDump(file_path) =>
                parse_functions(&mut fun_ctx, &file_path),
        }
    }
}

type TypParseContext = ();

fn parse_types(parse_context: &mut TypParseContext, file_path: &Path) {
    println!("Skipping {}...", file_path.display())
}

type FunParseContext = ();

fn parse_functions(parse_context: &mut FunParseContext, file_path: &Path) {
    println!("Skipping {}...", file_path.display())
}
