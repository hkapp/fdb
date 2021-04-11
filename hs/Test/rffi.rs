#![crate_type = "lib"]

#[no_mangle]
pub extern fn bar(x: f64) -> f64 {
    x - 1.0
}
