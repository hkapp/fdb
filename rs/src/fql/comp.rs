mod dataflow;

use super::{QPlan, RuntimeError, QVal};
use super::sqlexec;
use crate::ctx::DbCtx;
use std::ops;
use crate::objstore;
use std::rc::Rc;
use crate::data;

/* Interpreter: preparation step */

pub enum Cursor {
    Read(CurRead),
    Filter(CurFilter)
}

pub struct CurRead {
    pub rowid_range: ops::Range<Rowid>,
    pub tab_name:    String,
}

pub struct CurFilter {
    pub pred_obj:     Rc<objstore::Obj>,
    pub child_cursor: Box<Cursor>,
}

type Rowid = u32;

fn max_rowid(tab_name: &str) -> Result<Rowid, RuntimeError> {
    let query = format!("SELECT MAX(ROWID) FROM {}", tab_name);
    let max_rowid = data::sql_one_row_one_col(&query)?;

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

fn filter_cursor(filter_fun: Rc<objstore::Obj>, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = to_cursor(&child_qplan, db_ctx)?;

    let fun_decl = super::extract_decl(&filter_fun)?;
    /* TODO move this check at cursor build time */
    super::check_is_fun_decl(fun_decl)?;

    let cur_filter =
        CurFilter {
            pred_obj:     filter_fun,
            child_cursor: Box::new(child_cursor)
        };

    let cursor = Cursor::Filter(cur_filter);
    Ok(cursor)
}

pub fn to_cursor(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    use QPlan::*;
    match qplan {
        ReadT(qreadt) =>
            read_cursor(&qreadt.tab_name),

        Filter(qfilter) =>
            filter_cursor(Rc::clone(&qfilter.filter_fun), &qfilter.qchild, db_ctx),
    }
}

/* Interpreter: execution step */

#[derive(Debug, Clone)]
pub struct ColId {  /* make private again if possible */
    col_name: String,
    tab_name: String
}

#[derive(Debug, Clone)]
pub struct DataGuide(Vec<ColId>);

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
