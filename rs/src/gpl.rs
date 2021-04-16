use std::path::{Path, PathBuf};
use core::iter;

use walkdir::WalkDir;

pub fn load_everything_under(root_dir: &Path) {
    println!("I'm doing nothing so far!");

    let mut hi_ctx = HiParseContext::default();
    let mut prep_ctx = PrepParseContext::default();

    for ghc_dump in all_dump_files(root_dir) {
        match ghc_dump {
            GhcDump::DumpHi(file_path)   => parse_hi_dump(&mut hi_ctx, &file_path),
            GhcDump::DumpPrep(file_path) => parse_prep_dump(&mut prep_ctx, &file_path),
        }
    }
}

fn all_dump_files(root_dir: &Path) -> impl Iterator<Item = GhcDump> {
    all_files_under(root_dir)
        .filter_map(as_ghc_dump)
}

// Owned version of Path
type PathString = PathBuf;

enum GhcDump {
    DumpHi(PathString),
    DumpPrep(PathString)
}

fn as_ghc_dump(file_md: walkdir::DirEntry) -> Option<GhcDump> {
    let file_name = file_md.path();

    let file_ext = file_name.extension()
                    .and_then(|os_ext| os_ext.to_str());
    if file_ext.is_none() {
        return None;
    }

    match file_ext.unwrap() {
        "dump-hi"   =>
            Some(
                GhcDump::DumpHi(
                    PathBuf::from(file_name))),

        "dump-prep" =>
            Some(
                GhcDump::DumpPrep(
                    PathBuf::from(file_name))),

        _           => None,
    }
}

fn all_files_under(dir: &Path) -> impl Iterator<Item = walkdir::DirEntry> {
    WalkDir::new(dir)
        .follow_links(false)
        .into_iter()
        .filter_map(|e| e.ok())
}

type HiParseContext = ();

fn parse_hi_dump(parse_context: &mut HiParseContext, file_path: &Path) {
    println!("Skipping {}...", file_path.display())
}

type PrepParseContext = ();

fn parse_prep_dump(parse_context: &mut PrepParseContext, file_path: &Path) {
    println!("Skipping {}...", file_path.display())
}
