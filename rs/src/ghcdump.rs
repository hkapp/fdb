use std::path::Path;

mod files;
mod parser;

pub fn load_everything_under(root_dir: &Path) {
    parser::parse_all(
        files::find_dump_files(root_dir))
}
