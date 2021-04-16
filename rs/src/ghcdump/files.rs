use std::path::{Path, PathBuf};

use walkdir::WalkDir;

pub fn find_dump_files(root_dir: &Path) -> impl Iterator<Item = DumpFile> {
    all_files_under(root_dir)
        .filter_map(as_ghc_dump)
}

// Owned version of Path
pub type PathString = PathBuf;

pub enum DumpFile {
    TypDump(PathString),
    FunDump(PathString)
}

fn as_ghc_dump(file_md: walkdir::DirEntry) -> Option<DumpFile> {
    let file_name = file_md.path();

    let file_ext = file_name.extension()
                    .and_then(|os_ext| os_ext.to_str());
    if file_ext.is_none() {
        return None;
    }

    match file_ext.unwrap() {
        "dump-hi"   =>
            Some(
                DumpFile::TypDump(
                    PathBuf::from(file_name))),

        "dump-prep" =>
            Some(
                DumpFile::FunDump(
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
