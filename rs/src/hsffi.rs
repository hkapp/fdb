use std::str;
use std::slice;
use std::os::raw::c_char;
use std::ptr;
use std::fmt::Debug;

use crate::ctx::DbCtx;
use crate::ctx;
use crate::fql::{self, QPlan, QVal};

#[allow(non_camel_case_types)]
type c_sizet = usize;

type QPlanPtr = *const QPlan;

/* GENERIC HASKELL FFI HELPERS */

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

/* If there was an error, print it and return NULL.
 * The Haskell runtime will abort and nicely free the memory.
 */
unsafe fn to_hs_res<T, E: Debug>(res: Result<T, E>) -> *const T {
    match res {
        Ok(val) =>
            to_hs_ptr(val),

        Err(e)  => {
            println!("{:?}", &e);
            ptr::null()
        }
    }
}

/* We trust Haskell to return us the same pointer */
unsafe fn borrow_hs_ptr<'a, T>(ptr: *const T) -> &'a T {
    &*ptr
}

/* We now want Rust to own this memory.
 * This must be a pointer that was allocated by Rust initially.
 */
unsafe fn own_hs_ptr<T>(ptr: *mut T) -> Box<T> {
    Box::from_raw(ptr)
}

/* This pointer was allocated by Rust, passed to Haskell,
 * and now the Haskell GC determined we need to free it.
 */
unsafe fn release_hs_ptr<T>(ptr: *mut T) {
    drop(own_hs_ptr(ptr))
}

/* DB FFI */

#[no_mangle]
pub extern fn initDB() -> *const DbCtx {
    let ctx = ctx::init_db();

    unsafe {
        to_hs_res(ctx)
    }
}

#[no_mangle]
pub extern fn release_ctx(ctx: *mut DbCtx) {
    println!("Releasing context...");

    unsafe {
        release_hs_ptr(ctx)
    }
}

/* QUERY FFI */

#[no_mangle]
pub extern fn release_qplan(qptr: *mut QPlan) {
    println!("Releasing the query plan...");

    unsafe {
        release_hs_ptr(qptr)
    }
}

#[no_mangle]
pub extern fn readT(_db: *const DbCtx, str_buf: *const c_char, str_len: c_sizet) -> QPlanPtr {
    let tab_name: &str = unsafe { str_from_hs(str_buf, str_len).unwrap() };
    /* String::from must copy the &str, or we might be in big trouble */
    /* TODO move to 'fql' */
    let qplan = fql::read_table(tab_name);

    unsafe {
        to_hs_ptr(qplan)
    }
}

#[no_mangle]
pub extern fn filterQ(
    db_ptr:        *const DbCtx,
    prev_plan_ptr: *const QPlan,
    fun_name_buf:  *const c_char,
    fun_name_len:  c_sizet)
    -> *const QPlan
{
    let db_ctx = unsafe {
        borrow_hs_ptr(db_ptr)
    };

    let prev_plan = unsafe {
        borrow_hs_ptr(prev_plan_ptr)
    };

    let fun_name: &str = unsafe {
        str_from_hs(fun_name_buf, fun_name_len)
            .unwrap()  /* do something more here? */
    };

    let new_plan = fql::filter(prev_plan, fun_name, db_ctx);

    unsafe {
        to_hs_res(new_plan)
    }
}

#[no_mangle]
pub extern fn execQ(db_ptr: *const DbCtx, plan_ptr: *const QPlan, buf_ptr: *mut QVal, n_alloc: c_sizet)
    -> c_sizet
{
    let db_ctx = unsafe {
        borrow_hs_ptr(db_ptr)
    };

    let qplan = unsafe {
        borrow_hs_ptr(plan_ptr)
    };

    let res_buf = unsafe {
        slice::from_raw_parts_mut(buf_ptr, n_alloc)
    };

    fql::exec_into(qplan, db_ctx, res_buf)
        .unwrap_or_else(|e| {
            println!("{:?}", e);
            0  /* TODO change to -1? requires moving from usize to isize */
        })
}
