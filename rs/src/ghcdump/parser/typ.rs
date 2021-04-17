use std::path::Path;

pub type TypInfo = ();

pub fn parse(_parse_context: &mut TypInfo, file_path: &Path) {
    println!("Skipping {}...", file_path.display())
}
