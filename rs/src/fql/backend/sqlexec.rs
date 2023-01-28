use crate::fql::{QVal, RuntimeError, QPlan};
use rusqlite as sqlite;
use crate::ctx::DbCtx;
use crate::ir;
use crate::tables::TABLE_PAIRS;
use std::collections::HashMap;
use crate::objstore;

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

        PatMatch(ir::PatMatch { matched_var: _, pat_case }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            let deconstruct = pat_case;
            let field_binds = &deconstruct.field_binds;

            for field_index in 0..field_binds.len() {
                let field_bind = field_binds.get(field_index).unwrap();

                if field_bind.is_none() {
                    /* this index is not bound */
                    continue;
                }
                let field_bind = field_bind.as_ref().unwrap();

                /* FIXME this only works for a single level of nesting */
                let sql_col = TABLE_PAIRS.columns
                                .get(field_index)
                                .unwrap()
                                .clone();
                let conflict = eval_state.insert(&field_bind, sql_col);

                if conflict.is_some() {
                    return Err(RuntimeError::ConflictingDefForVar(field_bind.clone()));
                }
            }

            rec_inline_filter_sql(&deconstruct.body, eval_state)
        }

        FunCall(ir::FunCall { operator, val_args }) => {
            use ir::Operator;
            match &operator {
                Operator::Noop => {
                    assert!(val_args.len() == 1);
                    let arg_name = val_args.get(0).unwrap();
                    let arg_sql = eval_state.get(arg_name).unwrap();
                    Ok(arg_sql.clone())
                }

                Operator::LessThanOrEqual => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let sql_left  = eval_state.get(arg_left).unwrap();
                    let sql_right = eval_state.get(arg_right).unwrap();

                    let sql = format!("{} <= {}", sql_left, sql_right);
                    Ok(sql)
                }

                Operator::Plus => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let sql_left  = eval_state.get(arg_left).unwrap();
                    let sql_right = eval_state.get(arg_right).unwrap();

                    let sql = format!("({} + {})", sql_left, sql_right);
                    Ok(sql)
                }
            }
        }

        LitVal(lit) => {
            match lit {
                ir::LitVal::IntLit(n) =>
                    Ok(n.to_string()),
            }
        }
    }
}

fn inline_filter_sql(filter_fun: &objstore::Obj, _db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    let fun_decl = super::extract_decl(&filter_fun)?;
    super::check_is_fun_decl(fun_decl)?;

    let mut eval_state = HashMap::default();
    rec_inline_filter_sql(&fun_decl.body, &mut eval_state)
}

fn rec_to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    use QPlan::*;
    let sql = match qplan {
        ReadT(qreadt) =>
            format!("SELECT * FROM {}", &qreadt.tab_name),

        Filter(qfilter) => {
            let rec_sql = rec_to_sql(&qfilter.qchild, db_ctx)?;
            let where_clause = inline_filter_sql(&qfilter.filter_fun, db_ctx)?;

            format!("SELECT * FROM ({}) WHERE {}",
                    rec_sql, where_clause)
        },

        Map(_qmap) => {
            return Err(RuntimeError::MapNotSupported {
                backend: String::from("in SQL backend")
            });
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
