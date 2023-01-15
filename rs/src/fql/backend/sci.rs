mod dataflow;
mod qeval;

pub use qeval::{exec_interpreter_into, RtVal};
use super::{DB_FILENAME, STRUCT_COL_PREFIX};

use crate::fql::{QPlan, RuntimeError};
use crate::ctx::DbCtx;
use std::ops;
use crate::objstore;
use std::rc::Rc;
use crate::data;
use crate::ir;

/* Interpreter: preparation step */

pub struct Cursor {
    pub pipe_count: usize,
    pub op_tree:    OpTree,
}

struct OpTree {
    output_fmt: RowFmt,
    row_op:     RowOp,
}

pub enum RowOp {
    TableScan(TableScan),
    Filter(FilterOp)
}

/* TODO rename */
pub struct TableScan {
    pub rowid_range: ops::Range<Rowid>,
    query:           String,
}

pub struct FilterOp {
    filter_code:  ir::Expr,
    child_cursor: Box<OpTree>,
}

type Rowid = u32;

fn max_rowid(tab_name: &str) -> Result<Rowid, RuntimeError> {
    let query = format!("SELECT MAX(ROWID) FROM {}", tab_name);
    let max_rowid = data::sql_one_row_one_col(&query)?;

    Ok(max_rowid)
}

fn ts_node(tab_name: &str) -> Result<OpTree, RuntimeError> {
    let max_rowid = max_rowid(tab_name)?;

    let rowid_range =
        ops::Range {
            start: 1 /* incl */,
            end: max_rowid+1 /* excl */
        };

    let query = data::all_columns_query(tab_name);

    let cur_read =
        TableScan {
            rowid_range,
            query
        };

    let row_op = RowOp::TableScan(cur_read);
    let op_tree = OpTree {
        output_fmt,
        row_op,
    };
    Ok(op_tree)
}

fn filter_node(filter_fun: Rc<objstore::Obj>, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<OpTree, RuntimeError>
{
    let subtree = build_op_tree(&child_qplan, db_ctx)?;

    let fun_decl = super::extract_decl(&filter_fun)?;
    let fun_body = super::check_is_fun_decl(fun_decl)?;

    let cur_filter =
        FilterOp {
            filter_code:  ir::Expr::AnonFun(fun_body.clone()),
            child_cursor: Box::new(subtree)
        };

    let row_op = RowOp::Filter(cur_filter);
    let op_tree = OpTree {
        output_fmt,
        row_op,
    };
    Ok(op_tree)
}

fn build_op_tree(qplan: &QPlan, db_ctx: &DbCtx) -> Result<OpTree, RuntimeError> {
    use QPlan::*;
    match qplan {
        TableScanT(qreadt) =>
            ts_node(&qreadt.tab_name),

        Filter(qfilter) =>
            filter_node(Rc::clone(&qfilter.filter_fun), &qfilter.qchild, db_ctx),

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
