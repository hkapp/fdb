/*! The runtime code for the [Late Materialization Interpreter][super]
 */

use crate::ir;
use crate::data;
use super::super::{RuntimeError, QVal, Status};
use crate::fql::backend::sqlexec;
use std::collections::HashMap;
use super::{Cursor, CurKind, CurRead, CurFilter, new_data_guide, DataGuide, ColId};

/* Interpreter: preparation step */

type Rowid = u32;

/* Interpreter: execution step */

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

fn read_column_value(column: &ColId, rowid: Rowid) -> Result<RtVal, RuntimeError>
{
    let query = format!("SELECT {} FROM {} WHERE ROWID = {}",
                        column.col_name, column.tab_name, rowid);
    let col_val = data::sql_one_row_one_col(&query)?;
    let rt_val = RtVal::UInt32(col_val);

    Ok(rt_val)
}

fn resolve_dataguide_entry(data_guide: &DataGuide, field_index: usize, rowid: Rowid)
    -> Result<RtVal, RuntimeError>
{
    let column = data_guide.0.get(field_index)
                    .ok_or_else(|| RuntimeError::IndexNotInDataGuide(field_index))?;

    read_column_value(column, rowid)
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

        PatMatch(ir::PatMatch { matched_var, pat_case }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            let deconstruct = pat_case;
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
                    RtVal::DataGuide(dg) => dg, /* TODO remove */
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

        FunCall(ir::FunCall { operator, val_args }) => {
            use ir::Operator;
            match &operator {
                Operator::Noop => {
                    assert!(val_args.len() == 1);
                    let arg_name = val_args.get(0).unwrap();
                    let arg_val = interpreter.get(arg_name).unwrap();
                    Ok(arg_val.clone())
                }

                Operator::LessThanOrEqual => {
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
                                RuntimeError::UnsupportedComparison3{
                                    left:  val_left.clone(),
                                    right: val_right.clone()
                                }),
                    }
                }

                Operator::Plus => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let val_left  = interpreter.get(arg_left).unwrap();
                    let val_right = interpreter.get(arg_right).unwrap();

                    match (val_left, val_right) {
                        (RtVal::UInt32(int_left), RtVal::UInt32(int_right)) =>
                            Ok(
                                RtVal::UInt32(int_left + int_right)),

                        _ =>
                            Err(
                                RuntimeError::UnsupportedAddition3{
                                    left:  val_left.clone(),
                                    right: val_right.clone()
                                }),
                    }
                }
            }
        }

        LitVal(lit) => {
            match lit {
                ir::LitVal::IntLit(n) =>
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

fn cursor_data_guide(cursor: &Cursor) -> Result<DataGuide, RuntimeError> {
    match &cursor.cur_kind {
        CurKind::Read(cur_read) =>
            new_data_guide(&cur_read.tab_name),

        CurKind::Filter(cur_filter) =>
            /* Filter: unchanged data guide (pass-through only) */
            cursor_data_guide(&cur_filter.child_cursor),
    }
}

fn cursor_fetch_filter(cur_filter: &mut CurFilter) -> Result<Option<Rowid>, RuntimeError> {
    /* For now, this code MUST be an anonymous function (no lowerings yet) */
    let pred_fun = match &cur_filter.filter_code {
        ir::Expr::AnonFun(anon_fun) => anon_fun,
        _ => return Err(RuntimeError::NotAFunction),
    };
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
                        RuntimeError::FilterNotBoolean3(val));
            }
        }
    }

    /* The child row was either Err(_) or Ok(None) */
    /* Return that row in either case */
    return child_row;
}

fn cursor_fetch(cursor: &mut Cursor) -> Result<Option<Rowid>, RuntimeError> {
    match &mut cursor.cur_kind {
        CurKind::Read(cur_read) =>
            cursor_fetch_read(cur_read),

        CurKind::Filter(cur_filter) =>
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
