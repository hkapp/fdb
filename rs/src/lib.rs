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

fn query_sqlite() -> Result<Vec<QVal>> {
    let conn = Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare("SELECT * FROM foo;")?;
    let mut rows = stmt.query([])?;
    let mut qres = vec!();

    /* TODO use collect instead */
    while let Some(row) = rows.next()? {
        qres.push(row.get(0)?);
    }

    Ok(qres)
}

type QResultPtr = *mut QResult;

type QVal = u32;

#[repr(C)]
pub struct QResult {
    length: usize,
    array:  *mut QVal
}

#[no_mangle]
pub extern fn execQ() -> QResultPtr {
    let mut rvec = query_sqlite().unwrap_or_default();

    let carray = QResult {
        length: rvec.len(),
        array:  rvec.as_mut_ptr()
    };

    /* Do not call rvec's destructor.
     * So the inner buffer will remain live.
     */
    std::mem::forget(rvec);

    Box::into_raw(Box::new(carray))
}

#[no_mangle]
pub extern fn closeQ(cptr: QResultPtr) {
    let qres = unsafe {
        Box::from_raw(cptr)
    };
    let _boxed_array = unsafe {
        Box::from_raw(qres.array)
    };
    println!("Closing the cursor...")
    /* Both pointers are dropped and freed here */
}
