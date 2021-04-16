use std::path::Path;

mod files;
mod parser;

pub type Ctx = parser::ParseResult;

pub fn load_everything_under(root_dir: &Path) -> Result<Ctx, parser::ParseErr> {
    parser::parse_all(
        files::find_dump_files(root_dir))
}
