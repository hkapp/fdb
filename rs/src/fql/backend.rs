pub mod sqlexec;
pub mod dri; /* Dynamically-typed Row-based Interpreter */
pub mod dci; /* Dynamically-typed Column-based Interpreter */
pub mod sci; /* Statically-typed Column-based Interpreter */

use crate::data::DB_FILENAME;
use crate::ir;
use crate::objstore::{self, Symbol};
use super::{CompileError, RuntimeError, QVal, Status};

/* Object store helpers */

fn check_is_fun_decl(decl: &ir::Decl) -> Result<&ir::AnonFun, super::CompileError> {
    use ir::Expr::*;
    match &decl.body {
        /* This is the only accepted case */
        AnonFun(anon_fun) => Ok(&anon_fun),

        /* Every other case is an error */
        _ => {
            /* Gather exactly what it was for the error message */
            let what = match &decl.body {
                FunCall(_)  => "Function call",
                LetExpr(_)  => "Let expression",
                PatMatch(_) => "Pattern matching",
                LitVal(_)   => "Literal value",

                AnonFun(_)  => unreachable!(),
            };
            let fun_name = Symbol::new(decl.name.0.clone());

            Err(
                CompileError::NotAFunction {
                    symbol:      fun_name,
                    resolves_to: String::from(what)
                })
        }
    }
}

fn extract_decl(obj: &objstore::Obj) -> Result<&ir::Decl, CompileError> {
    obj.as_result()
        .map_err(|e|
            match e {
                objstore::FailedObj::ParseError(err_msg) => {
                    let symbol = obj.obj_name();
                    println!("Object \"{}\" has parsing errors:", symbol);
                    println!("{}", &err_msg);
                    CompileError::ObjectHasErrors(symbol.clone())
                }
            }
        )
}

pub struct BufWriter<'a, T> {
    buffer:  &'a mut [T],
    cur_pos: usize
}

impl<'a, T> BufWriter<'a, T> {
    fn new(target_buf: &'a mut [T]) -> Self {
        BufWriter {
            buffer:  target_buf,
            cur_pos: 0
        }
    }

    fn push(&mut self, val: T) {
        self.buffer[self.cur_pos] = val;
        self.cur_pos += 1;
    }
}
