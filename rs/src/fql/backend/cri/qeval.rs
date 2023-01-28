/*!
 * Execution of the sci interpreter
 *
 * The communication between the row op nodes is done via a columnar format.
 * At cursor compilation time, abstract block stream identifiers, called [Pipe]s are identified.
 * These stream identifiers are used at runtime to read from the proper main memory data block.
 * Each row op performs its action on every entry of its children blocks.
 * Currently, block size = 1;
 *
 * The interpretation of the function code is still done on a runtime-typed [RtVal].
 * There are no data-flow optimizations done yet.
 * This means that every row op writes its full output every time, even for pass-through columns.
 */

/* TODO
 *   compute the pipes at compile time
 *   add types
 *   support map
 *   support multi-row blocks
 *     all rows of same #values
 *     all rows of same byte size
 *   read values from blocks directly in the interpreter (avoid materializing row values)
 *   avoid copy for read-through columns (on map, not on filter)
 */
/* Workaround for Rust limitation, see https://github.com/rust-lang/rust/issues/29036 */
use super::super::cri;
use super::super::super::backend;
use backend::{RuntimeError, QVal, Status, dri, BufWriter};
use cri::{Cursor, RowOp, FilterOp, TableScan, OpTree, RowFmt, Pipe};
use crate::data;
pub use dri::{RtVal, RtStruct};
use crate::ir;
use std::cell::{RefCell, RefMut, Ref};

/* BlockMgr */

struct BlockMgr {
    mapping: Vec<RefCell<Option<Block>>>
}

// Necessary because Option<Block> does not have Clone to use `vec![None; size]` syntax
macro_rules! vec_repval {
    ($val: expr; $n: expr) => {
        {
            let n = $n;
            let mut vec = Vec::with_capacity(n);
            for _ in 0..n {
                vec.push($val);
            }
            vec
        }
    };
}

macro_rules! ensure_block_init {
    ($mb_block: expr, $typ: ident) => {
        {
            let mb_ref: &mut Option<Block> = $mb_block;
            if mb_ref.is_none() {
                *mb_ref = Some(Block::$typ(Vec::new()));
            }
            match mb_ref.as_mut().unwrap() {
                Block::$typ(vec) => Ok(vec),
                _                => Err(RuntimeError::IncorrectBlockType),
            }
        }
    };
}

impl BlockMgr {
    fn new(pipe_count: usize) -> Self {
        BlockMgr {
            mapping: vec_repval![RefCell::new(None); pipe_count]
        }
    }

    fn write_ref(&self, pipe: Pipe) -> RefMut<Option<Block>> {
        let refmut = self.mapping[pipe.idx].borrow_mut();
        refmut
    }

    fn read_block(&self, pipe: Pipe) -> Ref<Block> {
        // It is a logic error to try reading a block before it's been written at least once
        Ref::map(
            self.mapping[pipe.idx]
                .borrow(),
            |x| x.as_ref().unwrap())
    }
}

enum Block {
    UInt32(Vec<u32>),
    Bool(Vec<bool>),
}

impl Block {
    fn read_as_rtval(&self, idx: usize) -> Option<RtVal> {
        match self {
            Block::Bool(vec)   => vec.get(idx).map(|b| RtVal::Bool(*b)),
            Block::UInt32(vec) => vec.get(idx).map(|n| RtVal::UInt32(*n)),
        }
    }
}

const BLOCK_SIZE: usize = 1;

impl RowFmt {
    fn pipe_iter(&self) -> Box<dyn Iterator<Item=&Pipe> + '_> {
        match self {
            RowFmt::Scalar(pipe)     => Box::from(Vec::into_iter(vec![pipe])),
            RowFmt::Composite(pipes) => Box::from(pipes.iter()),
        }
    }
}

fn pull_ts(cur_read: &mut TableScan, block_mgr: &BlockMgr) -> Result<Option<usize>, RuntimeError> {
    cur_read.rowid_range
        .next()
        .map(|rowid| {
            let sql_row: Vec<u32> = data::table_row(&cur_read.tab_name, rowid)?;
            for (pipe, col_val) in cur_read.output_fmt()
                                    .pipe_iter()
                                    .zip(sql_row.into_iter())
            {
                let mut block_ref = block_mgr.write_ref(*pipe);
                let block_data = ensure_block_init!(&mut block_ref, UInt32)?;
                // Note we do blocks of size 1 for now
                assert_eq!(BLOCK_SIZE, 1);
                block_data.clear();
                block_data.push(col_val);
            }
            Ok(1 as usize)
        })
        .transpose()
}

fn interpret_fun(fun: &ir::AnonFun, row_arg: RtVal) -> Result<RtVal, RuntimeError> {
    dri::interpret_row_fun(fun, row_arg)
}

fn write_scalar_in_block(block: &mut Option<Block>, val: &RtVal) -> Result<(), RuntimeError> {
    assert_eq!(BLOCK_SIZE, 1);

    fn write<T>(vec: &mut Vec<T>, val: T) -> Result<(), RuntimeError> {
        vec.clear();
        vec.push(val);
        Ok(())
    }

    match val {
        RtVal::Bool(b) => {
            write(ensure_block_init!(block, Bool)?, *b)
        }
        RtVal::UInt32(n) => {
            write(ensure_block_init!(block, UInt32)?, *n)
        }
        _ => {
            Err(RuntimeError::CantWriteStructInBlock)
        }
    }
}

impl RowFmt {
    fn write_val(&self, rt_val: &RtVal, block_mgr: &BlockMgr) -> Result<(), RuntimeError> {
        match self {
            RowFmt::Scalar(pipe) => {
                write_scalar_in_block(&mut block_mgr.write_ref(*pipe), rt_val)
            },
            RowFmt::Composite(pipes) => {
                match rt_val {
                    RtVal::Struct(RtStruct { fields }) => {
                        for (p, f) in pipes.iter().zip(fields.iter()) {
                            write_scalar_in_block(&mut block_mgr.write_ref(*p), f)?;
                        }
                        Ok(())
                    }
                    _ => {
                        Err(RuntimeError::MismatchedTypes)
                    }
                }
            }
        }
    }
}

fn pull_filter(filter_op: &mut FilterOp, block_mgr: &BlockMgr) -> Result<Option<usize>, RuntimeError> {
    /* For now, this code MUST be an anonymous function (no lowerings yet) */
    let pred_fun = match &filter_op.filter_code {
        ir::Expr::AnonFun(anon_fun) => anon_fun,
        _ => return Err(RuntimeError::NotAFunction),
    };
    let child_cursor = &mut filter_op.child_cursor;

    assert_eq!(BLOCK_SIZE, 1);
    match next_row(child_cursor, block_mgr)? {
        Some(row_val) => {
            match interpret_fun(pred_fun, row_val.clone())? {
                RtVal::Bool(b) => {
                    if b {
                        /* Filter passed, write value */
                        filter_op.output_fmt().write_val(&row_val, block_mgr)?;
                        Ok(Some(1))
                    }
                    else {
                        /* Filter failed */
                        /* We have seen some rows, but filtered out */
                        Ok(Some(0))
                    }
                },

                val@_ => {
                    Err(RuntimeError::FilterNotBooleanSci(val))
                }
            }
        }
        None => Ok(None)
    }
}

fn cursor_pull(cursor: &mut RowOp, block_mgr: &BlockMgr) -> Result<Option<usize>, RuntimeError> {
    match cursor {
        RowOp::TableScan(cur_read) =>
            pull_ts(cur_read, block_mgr),

        RowOp::Filter(filter_op) =>
            pull_filter(filter_op, block_mgr),
    }
}

impl RowFmt {
    fn build_val(&self, block_mgr: &BlockMgr) -> RtVal {
        assert_eq!(BLOCK_SIZE, 1);
        use RowFmt::*;

        fn scalar(block_mgr: &BlockMgr, pipe: Pipe) -> RtVal {
            /* Without BLOCK_SIZE == 1, this unwrap is not "safe" */
            block_mgr
                .read_block(pipe)
                .read_as_rtval(0)
                .unwrap()
        }

        match self {
            Scalar(pipe)     => scalar(block_mgr, *pipe),
            Composite(pipes) => {
                let field_vals = pipes.iter()
                                    .map(|p| scalar(block_mgr, *p))
                                    .collect();
                RtVal::Struct(RtStruct { fields: field_vals })
            },
        }
    }
}

/* FIXME this won't work with multi-row */
fn next_row(op_tree: &mut RowOp, block_mgr: &BlockMgr) -> Result<Option<RtVal>, RuntimeError> {
    loop {
        match cursor_pull(op_tree, block_mgr)? {
            Some(1) => {
                let row_val = op_tree.output_fmt().build_val(block_mgr);
                return Ok(Some(row_val));
            }
            Some(0) => {
                /* rows were filtered out */
                /* try again */
            }
            Some(n) => {
                return Err(RuntimeError::MultiRowNotSupported(n));
            }
            None => {
                return Ok(None)
            }
        }
    }
}

pub fn exec_interpreter_into(cursor: &mut Cursor, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let mut rowcount = 0;
    let mut buf_writer = BufWriter::new(res_buf);
    let block_mgr = BlockMgr::new(cursor.pipe_count);
    while let Some(rt_val) = next_row(&mut cursor.op_tree, &block_mgr)? {
        dri::write_rtval_to_buffer(rt_val, &mut buf_writer)?;
        rowcount += 1;
    }
    Ok(rowcount)
}
