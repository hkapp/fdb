use super::{QVal, RuntimeError, QPlan};
use rusqlite as sqlite;
use crate::ctx::DbCtx;
use crate::ghcdump::ir;
use std::collections::HashMap;
use crate::objstore::Symbol;

/* Conversion QPlan -> SQL */

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
                return Err(RuntimeError::ConflictingDefForVar(var_name.clone()));
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
                let sql_col = format!("{}{}", super::STRUCT_COL_PREFIX, field_index);
                let conflict = eval_state.insert(&field_bind, sql_col);

                if conflict.is_some() {
                    return Err(RuntimeError::ConflictingDefForVar(field_bind.clone()));
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
    let fun_obj = super::resolve_symbol(fun_name, db_ctx)?;
    let fun_decl = super::extract_decl(&fun_obj)?;
    super::check_is_fun_decl(fun_decl)?;

    let mut eval_state = HashMap::default();
    rec_inline_filter_sql(&fun_decl.body, &mut eval_state)
}

fn rec_to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    use QPlan::*;
    let sql = match qplan {
        Read { tab_name } =>
            format!("SELECT * FROM {}", tab_name),

        Filter { fun_name, qchild: rec_qplan } => {
            let rec_sql = rec_to_sql(&rec_qplan, db_ctx)?;
            let where_clause = inline_filter_sql(&fun_name, db_ctx)?;

            format!("SELECT * FROM ({}) WHERE {}",
                    rec_sql, where_clause)
        },
    };
    Ok(sql)
}

pub fn to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    let mut sql = rec_to_sql(qplan, db_ctx)?;
    sql.push_str(";");
    Ok(sql)
}

/* SQL execution and marshalling */

pub fn query_sqlite_into(query: &str, res_buf: &mut [QVal]) -> Result<usize, RuntimeError> {
    let conn = sqlite::Connection::open(super::DB_FILENAME)?;

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
