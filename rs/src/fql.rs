use crate::objstore;
use crate::ctx::DbCtx;
use crate::objstore::Symbol;
use rusqlite as sqlite;
use crate::ghcdump::ir;
use std::collections::HashMap;
use std::ops;
use std::rc::Rc;

#[derive(Clone)]
pub enum QPlan {
    Read (String),
    Filter (Symbol, Box<QPlan>)
}

pub type QVal = u32;

#[derive(Debug)]
pub enum CompileError {
  SymbolNotDefined(Symbol),
  ObjectHasErrors(Symbol),
  NotAFunction { symbol: Symbol, resolves_to: String },
}

/* QUERY CONSTRUCTION */

pub fn filter(prev_plan: &QPlan, fun_name: &str, db_ctx: &DbCtx) -> Result<QPlan, CompileError> {
    let symbol =
        objstore::Symbol::new(
            String::from(fun_name));

    match db_ctx.obj_store.find(&symbol) {
        Some(obj) => {
            match obj.as_result() {
                /* TODO we should also check that the declaration at the end is actually a function
                 * and not a constant.
                 */
                Ok(_) => (),

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
    }

    let prev_plan_cp = Box::new(prev_plan.clone());
    let new_plan = QPlan::Filter(symbol, prev_plan_cp);
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
  UnsupportedFunction(ir::Global),
  ConflictingDefForVar(String),
  TooManyCases(usize),
  BufferTooSmall(usize),
}

const DB_FILENAME: &str = "../data/fdb.db";

fn query_sqlite_into(query: &str, res_buf: &mut [QVal]) -> Result<usize, RuntimeError> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare(query)?;
    let mut rows = stmt.query([])?;
    let col_count = rows.column_count().unwrap();
    let mut arr_pos = 0;

    /* For each row */
    while let Some(row) = rows.next()? {
        /* For each column */
        for col_idx in 0..col_count {
            if arr_pos >= res_buf.len() {
                return Err(RuntimeError::BufferTooSmall(arr_pos));
            }

            res_buf[arr_pos] = row.get(col_idx)?;
            arr_pos += 1;
        }
    }

    let row_count = ((arr_pos - 1) / col_count) + 1;
    Ok(row_count)
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
                LitConv(_)  => "Literal conversion",

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

const STRUCT_COL_PREFIX: &str = "col";

fn rec_inline_filter_sql<'a>(
    expr:       &'a ir::Expr,
    eval_state: &mut HashMap<&'a ir::Local, String>)
    -> Result<String, RuntimeError>
{
    use ir::Expr::*;
    match expr {
        /* TODO move this case up in the caller */
        AnonFun(ir::AnonFun { val_params, body, .. }) => {
            /* We only have one column for now */
            if val_params.len() != 1 {
                Err(RuntimeError::TooManyArguments(val_params.len()))
            }
            else {
                let param = val_params.get(0).unwrap();
                let param_name: &ir::Local = &param.name;
                let column_name = String::from("col0");

                let conflict = eval_state.insert(param_name, column_name);
                assert!(conflict.is_none());

                rec_inline_filter_sql(body, eval_state)
            }
        }

        LetExpr(ir::LetExpr { var_name, var_value, body, .. }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            let sql_val = rec_inline_filter_sql(var_value, eval_state)?;

            let conflict = eval_state.insert(var_name, sql_val);

            if conflict.is_some() {
                return Err(RuntimeError::ConflictingDefForVar(conflict.unwrap()));
            }

            rec_inline_filter_sql(body, eval_state)
        }

        PatMatch(ir::PatMatch { matched_var: _, pat_cases }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            if pat_cases.len() > 1 {
                /* We don't support actual ADTs right now with the SQL backend
                 * Only structs
                 */
                return Err(RuntimeError::TooManyCases(pat_cases.len()));
            }

            let deconstruct = pat_cases.get(0).unwrap();
            let field_binds = &deconstruct.field_binds;

            for field_index in 0..field_binds.len() {
                let field_bind = field_binds.get(field_index).unwrap();

                if field_bind.is_none() {
                    /* this index is not bound */
                    continue;
                }
                let field_bind = field_bind.as_ref().unwrap();

                /* FIXME this only works for a single level of nesting */
                let sql_col = format!("{}{}", STRUCT_COL_PREFIX, field_index);
                let conflict = eval_state.insert(&field_bind, sql_col);

                if conflict.is_some() {
                    return Err(RuntimeError::ConflictingDefForVar(conflict.unwrap()));
                }
            }

            rec_inline_filter_sql(&deconstruct.body, eval_state)
        }

        FunCall(ir::FunCall { called_fun, val_args, .. }) => {
            /* Do we know this function? */
            match &called_fun.0[..] {
                "GHC.Num.fromInteger" => {
                    assert!(val_args.len() == 1);
                    let arg_name = val_args.get(0).unwrap();
                    let arg_sql = eval_state.get(arg_name).unwrap();
                    Ok(arg_sql.clone())
                }

                "GHC.Classes.<=" => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let sql_left  = eval_state.get(arg_left).unwrap();
                    let sql_right = eval_state.get(arg_right).unwrap();

                    let sql = format!("{} <= {}", sql_left, sql_right);
                    Ok(sql)
                }

                _ => {
                    Err(
                        RuntimeError::UnsupportedFunction(called_fun.clone()))
                }
            }
        }

        LitConv(ir::LitConv { raw_lit, .. }) => {
            match raw_lit {
                ir::RawLit::IntLit(n) =>
                    Ok(n.to_string()),
            }
        }
    }
}

fn inline_filter_sql(fun_name: &Symbol, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    let fun_obj = resolve_symbol(fun_name, db_ctx)?;
    let fun_decl = extract_decl(&fun_obj)?;
    check_is_fun_decl(fun_decl)?;

    let mut eval_state = HashMap::default();
    rec_inline_filter_sql(&fun_decl.body, &mut eval_state)
}

fn rec_to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    use QPlan::*;
    let sql = match qplan {
        Read(tab_name) =>
            format!("SELECT * FROM {}", tab_name),

        Filter(fun_name, rec_qplan) => {
            let rec_sql = rec_to_sql(&rec_qplan, db_ctx)?;
            let where_clause = inline_filter_sql(&fun_name, db_ctx)?;

            format!("SELECT * FROM ({}) WHERE {}",
                    rec_sql, where_clause)
        },
    };
    Ok(sql)
}

fn to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    let mut sql = rec_to_sql(qplan, db_ctx)?;
    sql.push_str(";");
    Ok(sql)
}

/* Interpreter */

enum Cursor {
    Read(ops::Range<Rowid>),
    Filter { pred_obj: Rc<objstore::Obj>, child_cursor: Box<Cursor> }
}

type Rowid = u32;

fn max_rowid(tab_name: &str) -> Result<Rowid, RuntimeError> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let query = format!("SELECT MAX(ROWID) FROM {}", tab_name);
    let mut stmt = conn.prepare(&query)?;
    let max_rowid = stmt.query_row([], |row| row.get(0))?;

    Ok(max_rowid)
}

fn read_cursor(tab_name: &str) -> Result<Cursor, RuntimeError> {
    let max_rowid = max_rowid(tab_name)?;

    let rowid_range =
        ops::Range {
            start: 1 /* incl */,
            end: max_rowid+1 /* excl */
        };

    let cursor = Cursor::Read(rowid_range);
    Ok(cursor)
}

fn filter_cursor(fun_name: &Symbol, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = to_cursor(&child_qplan, db_ctx)?;

    let pred_obj = {
        let fun_obj = resolve_symbol(fun_name, db_ctx)?;
        let fun_decl = extract_decl(&fun_obj)?;
        check_is_fun_decl(fun_decl)?;

        fun_obj
    };

    let cursor =
        Cursor::Filter {
            pred_obj,
            child_cursor: Box::new(child_cursor)
        };
    Ok(cursor)
}

fn to_cursor(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    use QPlan::*;
    match qplan {
        Read(tab_name) =>
            read_cursor(&tab_name),

        Filter(fun_name, child_qplan) =>
            filter_cursor(&fun_name, &child_qplan, db_ctx),
    }
}

/* TODO add enum codes like "HasMoreEntries" */
type Status = usize;

pub fn exec_into(qplan: &QPlan, db_ctx: &DbCtx, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let sql_query = to_sql(qplan, db_ctx)?;
    println!("{}", sql_query);

    query_sqlite_into(&sql_query, res_buf)
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
