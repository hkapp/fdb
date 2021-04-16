#![crate_type = "lib"]
use rusqlite as sqlite;
use std::str;
use std::slice;
use std::os::raw::c_char;
use std::path::Path;

mod gpl;

#[allow(non_camel_case_types)]
type c_sizet = usize;

type QVal = u32;

type QPlanPtr = *const QPlan;

pub enum QPlan {
    Read (String),
    Filter
}

type HsStrBuf = *const c_char;
type HsStrLen = c_sizet;

/* Strings from Haskell are always borrowed */
unsafe fn str_from_hs<'a>(str_buf: HsStrBuf, str_len: HsStrLen)
    -> Result<&'a str, str::Utf8Error>
{
    let useable_slice = slice::from_raw_parts(str_buf as *const u8, str_len);
    str::from_utf8(useable_slice)
}

/* Pointers returned to Haskell will be owned by the Haskell runtime.
 * They will call the corresponding "release" function upon GC.
 */
unsafe fn to_hs_ptr<T>(val: T) -> *const T {
    Box::into_raw(Box::new(val))
}

/* We trust Haskell to return us the same pointer */
unsafe fn from_hs_ptr<'a, T>(ptr: *const T) -> &'a T {
    &*ptr
}

/* This pointer was allocated by Rust, passed to Haskell,
 * and now the Haskell GC determined we need to free it.
 */
unsafe fn release_hs_ptr<T>(ptr: *mut T) {
    drop(own_hs_ptr(ptr))
}

/* We now want Rust to own this memory.
 * This must be a pointer that was allocated by Rust initially.
 */
unsafe fn own_hs_ptr<T>(ptr: *mut T) -> Box<T> {
    Box::from_raw(ptr)
}

#[no_mangle]
pub extern fn readT(str_buf: *const c_char, str_len: c_sizet) -> QPlanPtr {
    let tab_name: &str = unsafe { str_from_hs(str_buf, str_len).unwrap() };
    /* String::from must copy the &str, or we might be in big trouble */
    let qplan = QPlan::Read(String::from(tab_name));

    unsafe { to_hs_ptr(qplan) }
}

#[no_mangle]
pub extern fn release_qplan(qptr: *mut QPlan) {
    println!("Releasing the query plan...");
    unsafe { release_hs_ptr(qptr) }
}

const DB_FILENAME: &str = "../data/fdb.db";

fn query_sqlite(query: &str) -> sqlite::Result<Vec<QVal>> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare(query)?;
    let mut rows = stmt.query([])?;
    let mut qres = vec!();

    /* TODO use collect instead */
    while let Some(row) = rows.next()? {
        qres.push(row.get(0)?);
    }

    Ok(qres)
}

fn query_sqlite_into(query: &str, res_buf: &mut [QVal]) -> sqlite::Result<usize> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare(query)?;
    let mut rows = stmt.query([])?;
    let mut arr_pos = 0;

    while let Some(row) = rows.next()? {
        res_buf[arr_pos] = row.get(0)?;
        arr_pos += 1;
    }

    Ok(arr_pos)
}

fn to_sql(qplan: &QPlan) -> String {
    use QPlan::*;
    match qplan {
        Read(tab_name) => format!("SELECT * FROM {};", tab_name),
        Filter          => String::from("error!")
    }
}

#[no_mangle]
pub extern fn execQ(plan_ptr: *const QPlan, buf_ptr: *mut QVal, n_alloc: c_sizet) -> c_sizet {
    let qplan = unsafe {
        from_hs_ptr(plan_ptr)
    };

    let sql_query = to_sql(qplan);
    println!("{}", sql_query);

    let res_buf = unsafe {
        slice::from_raw_parts_mut(buf_ptr, n_alloc)
    };

    query_sqlite_into(&sql_query, res_buf).unwrap()
}

type DbCtx = ();

const GHC_DUMP_DIR: &str = "../hs/bin/src";

#[no_mangle]
pub extern fn initDB() -> *const DbCtx {
    gpl::load_everything_under(&Path::new(GHC_DUMP_DIR));
    unsafe { to_hs_ptr(()) }
}

#[no_mangle]
pub extern fn release_ctx(ctx: *mut DbCtx) {
    println!("Releasing context...");
    unsafe { release_hs_ptr(ctx) }
}
