use std::path::Path;

pub type TypInfo = ();

pub fn parse(parse_context: &mut TypInfo, file_path: &Path) {
    println!("Skipping {}...", file_path.display())
}
