mod dataflow;

use crate::ghcdump::ir;
use super::{QPlan, RuntimeError, QVal, Status};
use super::sqlexec;
use crate::ctx::DbCtx;
use std::ops;
use crate::objstore::{self, Symbol};
use rusqlite as sqlite;
use std::rc::Rc;
use std::collections::HashMap;

/* Interpreter: preparation step */

pub enum Cursor {
    Read(CurRead),
    Filter(CurFilter)
}

pub struct CurRead {
    rowid_range: ops::Range<Rowid>,
    tab_name:    String,
}

pub struct CurFilter {
    pred_obj:     Rc<objstore::Obj>,
    child_cursor: Box<Cursor>,
}

type Rowid = u32;

fn sql_one_row_one_col<T>(query: &str) -> Result<T, RuntimeError>
    where
        T: sqlite::types::FromSql
{
    let conn = sqlite::Connection::open(super::DB_FILENAME)?;

    let mut stmt = conn.prepare(&query)?;
    stmt.query_row([], |row| row.get(0))
        .map_err(Into::into)
}

fn max_rowid(tab_name: &str) -> Result<Rowid, RuntimeError> {
    let query = format!("SELECT MAX(ROWID) FROM {}", tab_name);
    let max_rowid = sql_one_row_one_col(&query)?;

    Ok(max_rowid)
}

fn read_cursor(tab_name: &str) -> Result<Cursor, RuntimeError> {
    let max_rowid = max_rowid(tab_name)?;

    let rowid_range =
        ops::Range {
            start: 1 /* incl */,
            end: max_rowid+1 /* excl */
        };

    let cur_read =
        CurRead {
            rowid_range,
            tab_name: String::from(tab_name)
        };

    let cursor = Cursor::Read(cur_read);
    Ok(cursor)
}

fn filter_cursor(fun_name: &Symbol, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = to_cursor(&child_qplan, db_ctx)?;

    let pred_obj = {
        let fun_obj = super::resolve_symbol(fun_name, db_ctx)?;
        let fun_decl = super::extract_decl(&fun_obj)?;
        super::check_is_fun_decl(fun_decl)?;

        fun_obj
    };

    let cur_filter =
        CurFilter {
            pred_obj,
            child_cursor: Box::new(child_cursor)
        };

    let cursor = Cursor::Filter(cur_filter);
    Ok(cursor)
}

pub fn to_cursor(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    use QPlan::*;
    match qplan {
        Read { tab_name } =>
            read_cursor(&tab_name),

        Filter { fun_name, qchild: child_qplan } =>
            filter_cursor(&fun_name, &child_qplan, db_ctx),
    }
}

/* Interpreter: execution step */

#[derive(Debug, Clone)]
struct ColId {
    col_name: String,
    tab_name: String
}

#[derive(Debug, Clone)]
pub struct DataGuide(Vec<ColId>);

#[derive(Debug, Clone)]
pub enum RtVal {
    UInt32(u32),
    Bool(bool),
    DataGuide(DataGuide)
}

type Interpreter<'a> = HashMap<&'a ir::Local, RtVal>;

fn cursor_fetch_read(cur_read: &mut CurRead) -> Result<Option<Rowid>, RuntimeError> {
    Ok(cur_read.rowid_range.next())
}

fn resolve_dataguide_entry(data_guide: &DataGuide, field_index: usize, rowid: Rowid)
    -> Result<RtVal, RuntimeError>
{
    let column = data_guide.0.get(field_index)
                    .ok_or_else(|| RuntimeError::IndexNotInDataGuide(field_index))?;

    let query = format!("SELECT {} FROM {} WHERE ROWID = {}",
                        column.col_name, column.tab_name, rowid);
    let col_val = sql_one_row_one_col(&query)?;
    let rt_val = RtVal::UInt32(col_val);

    Ok(rt_val)
}

fn rec_interpret_row_expr<'a>(expr: &'a ir::Expr, rowid: Rowid, interpreter: &mut Interpreter<'a>)
    -> Result<RtVal, RuntimeError>
{
    use ir::Expr::*;
    match expr {
        AnonFun(_) =>
            Err(
                RuntimeError::UnsupportedExpression(
                    String::from("Anonymous function"))),

        LetExpr(ir::LetExpr { var_name, var_value, body, .. }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            let rt_val = rec_interpret_row_expr(var_value, rowid, interpreter)?;

            let conflict = interpreter.insert(var_name, rt_val);

            if conflict.is_some() {
                return Err(RuntimeError::ConflictingDefForVar(var_name.clone()));
            }

            rec_interpret_row_expr(body, rowid, interpreter)
        }

        PatMatch(ir::PatMatch { matched_var, pat_cases }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            if pat_cases.len() > 1 {
                /* We don't support actual ADTs right now in the interpreter
                 * Only structs
                 */
                return Err(RuntimeError::TooManyCases(pat_cases.len()));
            }

            let deconstruct = pat_cases.get(0).unwrap();
            let field_binds = &deconstruct.field_binds;

            /* For each field in the struct, assign the field value to a local variable.
             * Example:
             *   match s {
             *     MyStruct(a, b, c) => f(a, b, c),
             *   }
             * Assign the actual field values to temp variables a, b and c, reading from s.
             * Then execute f, which will read the variables a, b and c.
             */
            for field_index in 0..field_binds.len() {
                let field_bind = field_binds.get(field_index).unwrap();

                if field_bind.is_none() {
                    /* this index is not bound */
                    continue;
                }
                let field_bind = field_bind.as_ref().unwrap();

                let matched_val = interpreter.get(&matched_var)
                                    .ok_or_else(|| RuntimeError::UndefinedVariable(
                                                        matched_var.clone()))?;
                let data_guide = match &matched_val {
                    RtVal::DataGuide(dg) => dg,
                    _ => return Err(RuntimeError::PatternMatchNonStruct(matched_var.clone())),
                };

                let field_val = resolve_dataguide_entry(data_guide, field_index, rowid)?;
                let conflict = interpreter.insert(&field_bind, field_val);

                if conflict.is_some() {
                    return Err(RuntimeError::ConflictingDefForVar(field_bind.clone()));
                }
            }

            rec_interpret_row_expr(&deconstruct.body, rowid, interpreter)
        }

        FunCall(ir::FunCall { called_fun, val_args, .. }) => {
            /* Do we know this function? */
            match &called_fun.0[..] {
                "GHC.Num.fromInteger" => {
                    assert!(val_args.len() == 1);
                    let arg_name = val_args.get(0).unwrap();
                    let arg_val = interpreter.get(arg_name).unwrap();
                    Ok(arg_val.clone())
                }

                "GHC.Classes.<=" => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let val_left  = interpreter.get(arg_left).unwrap();
                    let val_right = interpreter.get(arg_right).unwrap();

                    match (val_left, val_right) {
                        (RtVal::UInt32(int_left), RtVal::UInt32(int_right)) =>
                            Ok(
                                RtVal::Bool(int_left <= int_right)),

                        _ =>
                            Err(
                                RuntimeError::UnsupportedComparison{
                                    left:  val_left.clone(),
                                    right: val_right.clone()
                                }),
                    }
                }

                _ =>
                    Err(
                        RuntimeError::UnsupportedFunction(called_fun.clone())),
            }
        }

        LitConv(ir::LitConv { raw_lit, .. }) => {
            match raw_lit {
                ir::RawLit::IntLit(n) =>
                    Ok(
                        RtVal::UInt32(*n as u32)),
            }
        }
    }
}

fn interpret_row_fun(predicate: &ir::AnonFun, rowid: Rowid, data_guide: &DataGuide)
    -> Result<RtVal, RuntimeError>
{
    let val_params = &predicate.val_params;
    if val_params.len() != 1 {
        Err(RuntimeError::TooManyArguments(val_params.len()))
    }
    else {
        let param = val_params.get(0).unwrap();
        let param_name: &ir::Local = &param.name;

        let mut interpreter: Interpreter = HashMap::new();

        let arg_value =
          if data_guide.0.len() == 1 {
            /* single column: treat as actual values
             * FIXME: need to consider type vs. newtype here
             */
            resolve_dataguide_entry(data_guide, 0, rowid)?
          }
          else {
            /* struct argument. Value is the data guide. */
            RtVal::DataGuide(data_guide.clone())
          };

        let conflict = interpreter.insert(param_name, arg_value);
        assert!(conflict.is_none());

        rec_interpret_row_expr(&predicate.body, rowid, &mut interpreter)
    }
}

fn new_data_guide(tab_name: &str) -> Result<DataGuide, RuntimeError> {
    fn dg_foo(tab_name: &str) -> DataGuide {
        let bar_col = ColId {
            col_name: String::from("bar"),
            tab_name: String::from(tab_name)
        };
        DataGuide(vec![bar_col])
    }

    fn dg_pairs(tab_name: &str) -> DataGuide {
        let ncols = 2;
        let mut cols = Vec::new();

        for col_idx in 0..ncols {
            let col_name = format!("{}{}", super::STRUCT_COL_PREFIX, col_idx);
            let column =
                ColId {
                    col_name,
                    tab_name: String::from(tab_name)
                };

            cols.push(column)
        }

        DataGuide(cols)
    }

    /* FIXME for now the table schemas are hardcoded */
    match tab_name {
      "foo"   => Ok(dg_foo(tab_name)),
      "pairs" => Ok(dg_pairs(tab_name)),
      _       => Err(RuntimeError::UnknownTable(String::from(tab_name))),
    }
}

fn cursor_data_guide(cursor: &Cursor) -> Result<DataGuide, RuntimeError> {
    match cursor {
        Cursor::Read(cur_read) =>
            new_data_guide(&cur_read.tab_name),

        Cursor::Filter(cur_filter) =>
            /* Filter: unchanged data guide (pass-through only) */
            cursor_data_guide(&cur_filter.child_cursor),
    }
}

fn cursor_fetch_filter(cur_filter: &mut CurFilter) -> Result<Option<Rowid>, RuntimeError> {
    let pred_decl = super::extract_decl(&cur_filter.pred_obj)?;
    let pred_fun = super::check_is_fun_decl(pred_decl)?;
    let child_cursor = &mut cur_filter.child_cursor;
    let data_guide = cursor_data_guide(child_cursor)?;
                    /* filter: data guide is unchanged (no map) */

    let mut child_row = cursor_fetch(child_cursor);
    while let Ok(Some(child_rowid)) = child_row {
        match interpret_row_fun(pred_fun, child_rowid, &data_guide)? {
            RtVal::Bool(b) => {
                if b {
                    /* Filter passed, return rowid */
                    return child_row;
                }
                else {
                    /* Filter failed */
                    /* Fetch the next row from the child, then loop */
                    child_row = cursor_fetch(child_cursor);
                }
            },

            val@_ => {
                return Err(
                        RuntimeError::FilterNotBoolean(val));
            }
        }
    }

    /* The child row was either Err(_) or Ok(None) */
    /* Return that row in either case */
    return child_row;
}

fn cursor_fetch(cursor: &mut Cursor) -> Result<Option<Rowid>, RuntimeError> {
    match cursor {
        Cursor::Read(cur_read) =>
            cursor_fetch_read(cur_read),

        Cursor::Filter(cur_filter) =>
            cursor_fetch_filter(cur_filter),
    }
}

fn gen_proj_query_from_dataguide(data_guide: &DataGuide, rowid: Rowid) -> String {
    /* 1. Generate a comma-separated list of column names */
    let col_names = data_guide.0
                        .iter()
                        .map(|e| &e.col_name as &str)
                        .collect::<Vec<&str>>()
                        .join(", ");
    /* FIXME add assert to check table name always the same */
    let table_name = &data_guide.0.get(0).unwrap().tab_name;

    let sql_query = format!("SELECT {} FROM {} WHERE rowid = {}",
                            col_names, table_name, rowid);

    return sql_query;
}

fn query_sqlite_rowid_into(rowid: Rowid, data_guide: &DataGuide, res_buf: &mut [QVal])
    -> Result<(), RuntimeError>
{
    let sql_query = gen_proj_query_from_dataguide(data_guide, rowid);
    let _nrows = sqlexec::query_sqlite_into(&sql_query, res_buf)?;
    /* TODO assert that returned number of rows is exactly one */
    Ok(())
}

pub fn exec_interpreter_into(cursor: &mut Cursor, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let data_guide = cursor_data_guide(cursor)?; /* TODO we should add the final "result conversion / out projection" node in the cursor tree */
    let ncols = data_guide.0.len();
    let mut rowcount = 0;
    while let Some(rowid) = cursor_fetch(cursor)? {
        /* TODO: read row values from rowid
         *   base code off of query_sqlite_into?
         */
        let buf_idx = ncols * rowcount;
        query_sqlite_rowid_into(rowid, &data_guide, &mut res_buf[buf_idx..])?;
        rowcount += 1;
    }
    Ok(rowcount)
}
