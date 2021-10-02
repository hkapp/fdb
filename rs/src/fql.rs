mod sqlexec;
mod interpreter;

use crate::ctx::DbCtx;
use crate::objstore::{self, Symbol};
use rusqlite as sqlite;
use crate::ir;
use std::rc::Rc;

const DB_FILENAME: &str = "../data/fdb.db";
const STRUCT_COL_PREFIX: &str = "col";

#[derive(Clone)]
pub enum QPlan {
    Read { tab_name: String },
    Filter { filter_fun: Rc<objstore::Obj>, qchild: Box<QPlan> }
}

pub type QVal = u32;

#[derive(Debug)]
pub enum CompileError {
  SymbolNotDefined(Symbol),
  ObjectHasErrors(Symbol),
  NotAFunction { symbol: Symbol, resolves_to: String },
}

/* QUERY CONSTRUCTION */

pub fn read_table(tab_name: &str) -> QPlan {
    QPlan::Read {
        tab_name: String::from(tab_name)
    }
}

pub fn filter(prev_plan: &QPlan, fun_name: &str, db_ctx: &DbCtx) -> Result<QPlan, CompileError> {
    let symbol =
        objstore::Symbol::new(
            String::from(fun_name));

    let resolved_fun =
        match db_ctx.obj_store.find(&symbol) {
            Some(obj) => {
                match obj.as_result() {
                    /* TODO we should also check that the declaration at the end is actually a function
                     * and not a constant.
                     */
                    Ok(_) => obj,

                    Err(objstore::FailedObj::ParseError(err_msg)) => {
                        println!("Object \"{}\" has parsing errors:", fun_name);
                        println!("{}", &err_msg);
                        return Err(
                            CompileError::ObjectHasErrors(symbol));
                    }
                }
            }

            None =>
                return Err(
                    CompileError::SymbolNotDefined(symbol)),
        };

    let prev_plan_cp = Box::new(prev_plan.clone());
    let new_plan = QPlan::Filter {
        filter_fun: resolved_fun,
        qchild:     prev_plan_cp
    };
                        /* need to clone to ensure Haskell doesn't ask
                         * the same memory to be cleaned twice */

    Ok(new_plan)
}

/* RUNTIME */

#[derive(Debug)]
pub enum RuntimeError {
  SqliteError(sqlite::Error),
  CompileError(CompileError),
  TooManyArguments(usize),
  ConflictingDefForVar(ir::Local),
  BufferTooSmall(usize),
  IndexNotInDataGuide(usize),
  UnsupportedExpression(String),
  UndefinedVariable(ir::Local),
  PatternMatchNonStruct(ir::Local),
  UnsupportedComparison { left: interpreter::RtVal, right: interpreter::RtVal },
  FilterNotBoolean(interpreter::RtVal),
  UnknownTable(String),
  ScalarRowFormatHasNoFields,
  FieldPathIncompletelyResolved
}

/* Object store helpers */

fn resolve_symbol(symbol: &Symbol, db_ctx: &DbCtx) -> Result<Rc<objstore::Obj>, CompileError> {
    /* Retrieve the declaration */
    db_ctx.obj_store.find(symbol)
        .ok_or_else(|| CompileError::SymbolNotDefined(symbol.clone()))
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

fn check_is_fun_decl(decl: &ir::Decl) -> Result<&ir::AnonFun, CompileError> {
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

/* TODO add enum codes like "HasMoreEntries" */
type Status = usize;

pub fn exec_into(qplan: &QPlan, db_ctx: &DbCtx, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let sqlite_backend = false;

    if sqlite_backend {
        /* Execute on full SQLite backend */
        let sql_query = sqlexec::to_sql(qplan, db_ctx)?;

        println!("SQLite execution:");
        println!("{}", sql_query);

        sqlexec::query_sqlite_into(&sql_query, res_buf)
    }
    else {
        /* Execute on current interpreter backend */
        let mut cursor = interpreter::to_cursor(qplan, db_ctx)?;

        println!("FDB interpreter:");
        interpreter::exec_interpreter_into(&mut cursor, res_buf)
    }
}

/* Error conversion */

impl From<sqlite::Error> for RuntimeError {
    fn from(sql_err: sqlite::Error) -> Self {
        RuntimeError::SqliteError(sql_err)
    }
}

impl From<CompileError> for RuntimeError {
    fn from(err: CompileError) -> Self {
        RuntimeError::CompileError(err)
    }
}
