//mod dataflow;
mod qeval;

pub use qeval::{exec_interpreter_into, RtVal};
use super::DB_FILENAME;

use crate::fql::{QPlan, RuntimeError};
use crate::ctx::DbCtx;
use crate::tables;
use std::ops;
use crate::objstore;
use std::rc::Rc;
use crate::data;
use crate::ir;
use data::RowId;

/* Interpreter: preparation step */

pub struct Cursor {
    pub pipe_count: usize,
    pub op_tree:    RowOp,
}

pub enum RowOp {
    TableScan(TableScan),
    Filter(FilterOp)
}

/* TODO rename */
pub struct TableScan {
    pub rowid_range: ops::Range<RowId>,
    tab_name:           String,
}

pub struct FilterOp {
    filter_code:  ir::Expr,
    child_cursor: Box<RowOp>,
}

/* RowFmt */

#[derive(Copy, Clone)]
struct Pipe {
    idx: usize
}

enum RowFmt {
    Scalar(Pipe),
    Composite(Vec<Pipe>),
}

trait OpTree {
    fn output_fmt(&self) -> RowFmt;
}

impl OpTree for RowOp {
    fn output_fmt(&self) -> RowFmt {
        use RowOp::*;
        match self {
            TableScan(ts) => ts.output_fmt(),
            Filter(filter) => filter.output_fmt(),
        }
    }
}

impl OpTree for FilterOp {
    fn output_fmt(&self) -> RowFmt {
        /* FIXME we need to increase the pipe numbers here */
        //self.child_cursor.output_fmt()
    }
}

impl OpTree for TableScan {
    fn output_fmt(&self) -> RowFmt {
        let table = tables::resolve(&self.tab_name)
                        .ok_or(RuntimeError::UnknownTable(String::from(&self.tab_name)))
                        .unwrap();
        match table.columns.len() {
            1   => {
                /* FIXME this allocation only works with a single Table Scan node
                 * in the entire QPlan
                 */
                RowFmt::Scalar(Pipe { idx: 0 })
            },
            n => {
                let pipes = (0..n)
                                .map(|idx| Pipe { idx })
                                .collect();
                RowFmt::Composite(pipes)
            },
        }
    }
}

/* Logic */

fn max_rowid(tab_name: &str) -> Result<RowId, RuntimeError> {
    let query = format!("SELECT MAX(ROWID) FROM {}", tab_name);
    let max_rowid = data::sql_one_row_one_col(&query)?;

    Ok(max_rowid)
}

fn ts_node(tab_name: &str) -> Result<RowOp, RuntimeError> {
    let max_rowid = max_rowid(tab_name)?;

    let rowid_range =
        ops::Range {
            start: 1 /* incl */,
            end: max_rowid+1 /* excl */
        };

    let cur_read =
        TableScan {
            rowid_range,
            tab_name: String::from(tab_name)
        };

    let row_op = RowOp::TableScan(cur_read);
    Ok(row_op)
}

fn filter_node(filter_fun: Rc<objstore::Obj>, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<RowOp, RuntimeError>
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
    Ok(row_op)
}

fn build_op_tree(qplan: &QPlan, db_ctx: &DbCtx) -> Result<RowOp, RuntimeError> {
    use QPlan::*;
    match qplan {
        ReadT(qreadt) =>
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

fn compute_pipe_count(op_tree: &RowOp) -> usize {
    fn fmt_pipe_count(fmt: &RowFmt) -> usize {
        match fmt {
            RowFmt::Scalar(_)         => 1,
            RowFmt::Composite(fields) => fields.len(), // we don't support nested structs yet
        }
    }

    let mut next_node = Some(op_tree);
    let mut pipe_count = 0;
    use RowOp::*;
    while let Some(curr_node) = next_node {
        pipe_count += fmt_pipe_count(&curr_node.output_fmt());
        next_node = match curr_node {
            TableScan(ts)  => None,
            Filter(filter) => Some(&filter.child_cursor),
        };
    }
    return pipe_count;
}

fn build_cursor(op_tree: RowOp, db_ctx: &DbCtx) -> Cursor {
    Cursor {
        pipe_count: compute_pipe_count(&op_tree),
        op_tree,
    }
}

pub fn full_compile(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    let op_tree = build_op_tree(qplan, db_ctx)?;
    let cursor = build_cursor(op_tree, db_ctx);
    /* TODO we're also supposed to get a full list of blocks to allocate here */
    //let final_dg /*(final_dg, alloc_plan)*/ = dataflow::apply_data_accesses(&mut cursor)?;
    //Ok((cursor, final_dg, alloc_plan))
    Ok(cursor)
}

/* Interpreter: execution step */

#[derive(Debug, Clone)]
pub struct ColId {  /* make private again if possible */
    col_name: String,
    tab_name: String
}
