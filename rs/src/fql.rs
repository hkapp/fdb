pub mod backend;

use crate::ctx::DbCtx;
use crate::objstore::{self, Symbol};
use rusqlite as sqlite;
use crate::ir;
use std::rc::Rc;
use backend::{sqlexec, dri, lmi, cri, Backend};

/* TODO rename QTree? */
/* naming: we can rename this phase or module "sprout"
 *   "to begin to grow; shoot forth, as a plant from a seed."
 */
#[derive(Clone)]
pub enum QPlan {
    ReadT(QReadT),
    Filter(QFilter),
    Map(QMap),
}

#[derive(Clone)]
pub struct QReadT {
    tab_name: String
}

#[derive(Clone)]
pub struct QFilter {
    filter_fun:  Rc<objstore::Obj>,
    qchild:      Box<QPlan>,
    //filter_code: Option<ir::Expr>,  /* TODO move only to comp::CFilter */
}

#[derive(Clone)]
pub struct QMap {
    map_fun:  Rc<objstore::Obj>,
    qchild:   Box<QPlan>,
}

pub enum SQPlan {
    Fold(SQFold),
}

pub struct SQFold {
    fold_fun:  Rc<objstore::Obj>,
    zero_fun:  Rc<objstore::Obj>,
    qchild:    Box<QPlan>,
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
    QPlan::ReadT(
        QReadT {
            tab_name: String::from(tab_name)
        }
    )
}

fn fun_obj(fun_name: &str, db_ctx: &DbCtx) -> Result<Rc<objstore::Obj>, CompileError> {
    let symbol =
        objstore::Symbol::new(
            String::from(fun_name));

    match db_ctx.obj_store.find(&symbol) {
        Some(obj) => {
            match obj.as_result() {
                /* TODO we should also check that the declaration at the end is actually a function
                 * and not a constant.
                 */
                Ok(_) => Ok(obj),

                Err(objstore::FailedObj::ParseError(err_msg)) => {
                    println!("Object \"{}\" has parsing errors:", fun_name);
                    println!("{}", &err_msg);
                    Err(
                        CompileError::ObjectHasErrors(symbol))
                }
            }
        }

        None =>
            Err(
                CompileError::SymbolNotDefined(symbol)),
    }
}

pub fn filter(prev_plan: &QPlan, fun_name: &str, db_ctx: &DbCtx) -> Result<QPlan, CompileError> {
    let resolved_fun = fun_obj(fun_name, db_ctx)?;

    let prev_plan_cp = Box::new(prev_plan.clone());
                        /* need to clone to ensure Haskell doesn't ask
                         * the same memory to be cleaned twice */
    let new_plan =
        QPlan::Filter(
            QFilter {
                filter_fun:  resolved_fun,
                qchild:      prev_plan_cp,
                //filter_code: None
            }
        );

    Ok(new_plan)
}

pub fn map(prev_plan: &QPlan, fun_name: &str, db_ctx: &DbCtx) -> Result<QPlan, CompileError> {
    let resolved_fun = fun_obj(fun_name, db_ctx)?;

    let prev_plan_cp = Box::new(prev_plan.clone());
                        /* need to clone to ensure Haskell doesn't ask
                         * the same memory to be cleaned twice */
    let new_plan =
        QPlan::Map(
            QMap {
                map_fun:  resolved_fun,
                qchild:   prev_plan_cp,
            }
        );

    Ok(new_plan)
}

pub fn fold(prev_plan: &QPlan, fun_name: &str, zero_name: &str, db_ctx: &DbCtx)
    -> Result<SQPlan, CompileError>
{
    let resolved_fun = fun_obj(fun_name, db_ctx)?;
    let resolved_zero = fun_obj(zero_name, db_ctx)?;

    let prev_plan_cp = Box::new(prev_plan.clone());
                        /* need to clone to ensure Haskell doesn't ask
                         * the same memory to be cleaned twice */
    let new_plan =
        SQPlan::Fold(
            SQFold {
                fold_fun: resolved_fun,
                zero_fun: resolved_zero,
                qchild:   prev_plan_cp,
            }
        );

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
  NoRowWithRowid(String, dri::Rowid),
  TooManyRowsWithRowid(String, dri::Rowid),
  CantWriteRtValToBuffer(dri::RtVal),
  UnsupportedComparison { left: dri::RtVal, right: dri::RtVal },
  UnsupportedAddition { left: dri::RtVal, right: dri::RtVal },
  FilterNotBoolean(dri::RtVal),
  UnsupportedComparison3 { left: lmi::RtVal, right: lmi::RtVal }, /* TODO: remove */
  UnsupportedAddition3 { left: lmi::RtVal, right: lmi::RtVal }, /* TODO: remove */
  FilterNotBoolean3(lmi::RtVal), /* TODO remove */
  FilterNotBooleanSci(cri::RtVal), /* TODO remove */
  UnknownTable(String),
  NotAFunction,
  MapNotSupported { backend: String },
  MultiRowNotSupported(usize),
  IncorrectBlockType,
  CantWriteStructInBlock,
  MismatchedTypes,
}

/* Object store helpers */

/*fn resolve_symbol(symbol: &Symbol, db_ctx: &DbCtx) -> Result<Rc<objstore::Obj>, CompileError> {
    /* Retrieve the declaration */
    db_ctx.obj_store.find(symbol)
        .ok_or_else(|| CompileError::SymbolNotDefined(symbol.clone()))
}*/

/* TODO add enum codes like "HasMoreEntries" */
type Status = usize;

pub fn exec_into(qplan: &QPlan, db_ctx: &DbCtx, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let curr_backend = Backend::Columnar;

    match curr_backend {
        Backend::SQLite => {
            /* Execute on full SQLite backend */
            let sql_query = sqlexec::to_sql(qplan, db_ctx)?;

            println!("SQLite execution:");
            println!("{}", sql_query);

            sqlexec::query_sqlite_into(&sql_query, res_buf)
        },

        Backend::NaiveInterpreter => {
            /* Execute on old naive dri */
            let mut cursor = dri::to_cursor(qplan, db_ctx)?;

            println!("Naive interpreter:");
            dri::exec_interpreter_into(&mut cursor, res_buf)
        },

        Backend::LazyMaterialize => {
            /* Execute on new columnar dri */
            let mut cursor = lmi::full_compile(qplan, db_ctx)?;

            println!("Lazy materialize interpreter:");
            lmi::exec_interpreter_into(&mut cursor, res_buf)
        },

        Backend::Columnar => {
            /* Execute on new columnar dri */
            let mut cursor = cri::full_compile(qplan, db_ctx)?;

            println!("Columnar interpreter:");
            cri::exec_interpreter_into(&mut cursor, res_buf)
        },
    }
}

pub fn execsq_into(qplan: &SQPlan, db_ctx: &DbCtx, res_buf: &mut QVal) -> Result<bool, RuntimeError> {
    let curr_backend = Backend::NaiveInterpreter;

    match curr_backend {
        Backend::SQLite => {
          Ok(false)
        },

        Backend::NaiveInterpreter => {
            /* Execute on old naive dri */
            let mut cursor = dri::to_sqcursor(qplan, db_ctx)?;

            println!("Naive interpreter:");
            dri::execsq_interpreter_into(&mut cursor, res_buf)
        },

        _ => {
            /* Execute on new columnar interpreters */
            /* TODO: not implemented for SQ */
            Ok(false)
        }
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
