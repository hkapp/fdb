mod dataflow;
mod qeval;

pub use qeval::{exec_interpreter_into, RtVal};
use super::DB_FILENAME;
use crate::tables::TABLE_PAIRS;
use crate::fql::{QPlan, RuntimeError};
use crate::ctx::DbCtx;
use std::ops;
use crate::objstore;
use std::rc::Rc;
use crate::data;
use crate::ir;

/* Interpreter: preparation step */

pub struct Cursor {
    pub out_pipe: Option<DataGuide>,  /* is this necessary? */
    pub cur_kind: CurKind,
}

pub enum CurKind {
    Read(CurRead), /* TODO rename enum entry */
    Filter(CurFilter)
}

/* TODO rename */
pub struct CurRead {
    pub rowid_range: ops::Range<Rowid>,
    pub tab_name:    String,
}

pub struct CurFilter {
    pub filter_code:  ir::Expr,
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

    let cur_kind = CurKind::Read(cur_read);
    let cursor = Cursor {
        cur_kind,
        out_pipe: None
    };
    Ok(cursor)
}

fn filter_cursor(filter_fun: Rc<objstore::Obj>, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = build_cursor(&child_qplan, db_ctx)?;

    let fun_decl = super::extract_decl(&filter_fun)?;
    /* TODO move this check at cursor build time */
    let fun_body = super::check_is_fun_decl(fun_decl)?;

    let cur_filter =
        CurFilter {
            filter_code:  ir::Expr::AnonFun(fun_body.clone()),
            child_cursor: Box::new(child_cursor)
        };

    let cur_kind = CurKind::Filter(cur_filter);
    let cursor = Cursor {
        cur_kind,
        out_pipe: None
    };
    Ok(cursor)
}

fn build_cursor(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    use QPlan::*;
    match qplan {
        ReadT(qreadt) =>
            read_cursor(&qreadt.tab_name),

        Filter(qfilter) =>
            filter_cursor(Rc::clone(&qfilter.filter_fun), &qfilter.qchild, db_ctx),

        Map(qmap) => {
            return Err(RuntimeError::MapNotSupported {
                backend: String::from("columnar interpreter")
            });
        }
    }
}

pub fn full_compile(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    let mut cursor = build_cursor(qplan, db_ctx)?;
    /* TODO we're also supposed to get a full list of blocks to allocate here */
    let final_dg /*(final_dg, alloc_plan)*/ = dataflow::apply_data_accesses(&mut cursor)?;
    //Ok((cursor, final_dg, alloc_plan))
    Ok(cursor)
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
        let cols = TABLE_PAIRS.columns
                        .iter()
                        .map(|col_name| ColId {
                            col_name: String::from(col_name),
                            tab_name: String::from(tab_name)
                        })
                        .collect();

        DataGuide(cols)
    }

    /* FIXME for now the table schemas are hardcoded */
    match tab_name {
      "foo"   => Ok(dg_foo(tab_name)),
      "pairs" => Ok(dg_pairs(tab_name)),
      _       => Err(RuntimeError::UnknownTable(String::from(tab_name))),
    }
}
