pub type FunInfo = ();
pub type FunErr = std::io::Error;

pub fn parse(parse_context: &mut FunInfo, input: &str) -> Result<(), FunErr> {
    println!("Skipping {} chars...", input.len());
    Ok (())
}
