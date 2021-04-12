#![crate_type = "lib"]
use rusqlite::{Connection, Result};

#[no_mangle]
pub extern fn bar(x: f64) -> f64 {
    match baz() {
        Ok(_) => println!("SQLite success"),
        Err(_) => println!("SQLite failure")
    };

    x - 1.0
}

const DB_FILENAME: &str = "../data/fdb.db";

fn baz() -> Result<()> {
    let conn = Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare("SELECT * FROM foo;")?;
    let mut rows = stmt.query([])?;

    while let Some(row) = rows.next()? {
        let x: u32 = row.get(0)?;
        println!("{}", x);
    }

    Ok(())
}
