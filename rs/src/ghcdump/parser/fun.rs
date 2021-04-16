use super::Parse;

pub type FunInfo = ();
pub type Err = String;

pub fn parse(parse_context: &mut FunInfo, input: &str) -> Result<(), Err> {
    println!("Skipping {} chars...", input.len());
    Ok (())
}
