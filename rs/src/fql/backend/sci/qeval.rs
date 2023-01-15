/**
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
 * This means that every row op writes its full output every time.
 */

use crate::ir;
use crate::data;
use super::super::{RuntimeError, QVal, Status, dri};
use crate::fql::backend::sqlexec;
use std::collections::HashMap;
use super::{Cursor, RowOp, FilterOp, TableScan, OpTree};
pub use dri::RtVal;
use std::cell::{RefCell, RefMut};

/* Interpreter: preparation step */

type Rowid = u32;

/* Interpreter: execution step */

#[derive(Debug, Clone)]
pub struct ColId {  /* make private again if possible */
    col_name: String,
    tab_name: String
}

/*
#[derive(Debug, Clone)]
pub struct DataGuide(Vec<ColId>);
*/
struct BlockMgr {
    mapping: Vec<RefCell<Option<Block>>>
};

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

macro_rules! typed_write_ref {
    ($bm: expr, $pipe: expr, $typ: ident) => {
        {
            let refmut = $bm.write_ref($pipe);
            refmut.map(|mb_block| {
                if mb_block.is_none() {
                    *mb_block = Some(Block::$typ(Vec::new()))
                }
                mb_block.as_mut_ref().uwnrap()
            })
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
        //fn any_new_block() -> Block {
            //Block::Bool(Vec::new())
        //}

        let refmut = self.mapping[pipe.idx].borrow_mut();
        refmut
        //refmut.map(|mb_block| {
            //match mb_block {
                //Some(block) => block,
                //None    => {
                    //*mb_block = Some()
                //}
            //}
        //})
    }
}

/*#[derive(Debug, Clone)]
pub enum RtVal {
    UInt32(u32),
    Bool(bool),
    DataGuide(DataGuide)
}*/

enum Block {
    UInt32(Vec<u32>),
    Bool(Vec<bool>),
}

type Interpreter<'a> = HashMap<&'a ir::Local, RtVal>;

const BLOCK_SIZE: usize = 1;

fn pull_ts(cur_read: &mut CurRead, block_mgr: &BlockMgr) -> Result<Option<usize>, RuntimeError> {
    fn execute_query(query: &str, rowid: Rowid) -> data::Row {
        data::execute_with_bind(query, rowid)
    }

    cur_read.rowid_range
        .next()
        .map(|rowid| {
            let sql_row = execute_query(cur_read.query, rowid)?;
            for (sql_col, pipe) in cur_read.spec {
                let col_val = sql_row.get(sql_col);
                let block = block_mgr.write_ref(pipe);
                // Note we do blocks of size 1 for now
                assert_eq!(BLOCK_SIZE, 1);
                block.clear();
                block.push(col_val);
            }
            Ok(1 as usize)
        })
        .transpose()
}

/*fn read_column_value(column: &ColId, rowid: Rowid) -> Result<RtVal, RuntimeError>
{
    let query = format!("SELECT {} FROM {} WHERE ROWID = {}",
                        column.col_name, column.tab_name, rowid);
    let col_val = data::sql_one_row_one_col(&query)?;
    let rt_val = RtVal::UInt32(col_val);

    Ok(rt_val)
}*/

/*fn resolve_dataguide_entry_(data_guide: &DataGuide, field_index: usize, rowid: Rowid)
    -> Result<RtVal, RuntimeError>
{
    let column = data_guide.0.get(field_index)
                    .ok_or_else(|| RuntimeError::IndexNotInDataGuide(field_index))?;

    read_column_value(column, rowid)
}*/

/*fn rec_interpret_row_expr<'a>(expr: &'a ir::Expr, rowid: Rowid, interpreter: &mut Interpreter<'a>)
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
                                RuntimeError::UnsupportedComparisonSci{
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
                                RuntimeError::UnsupportedAdditionSci{
                                    left:  val_left.clone(),
                                    right: val_right.clone()
                                }),
                    }
                }

                _ => {
                    Err(RuntimeError::UnsupportedOperator(operator.clone()))
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
}*/

fn interpret_fun(fun: &ir::AnonFun, row_arg: RtVal) -> Result<RtVal, RuntimeError> {
    dri::interpret_row_fun(fun, row_arg)
}

fn interpret_row_fun(predicate: &ir::AnonFun, block_mgr: &BlockMgr, row_format: &RowFmt)
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

        let arg_value = row_format.build_value(block_mgr);
          /*if data_guide.0.len() == 1 {
            /* single column: treat as actual values
             * FIXME: need to consider type vs. newtype here
             */
            resolve_dataguide_entry(data_guide, 0, rowid)?
          }
          else {
            /* struct argument. Value is the data guide. */
            RtVal::DataGuide(data_guide.clone())
          };*/

        let conflict = interpreter.insert(param_name, arg_value);
        assert!(conflict.is_none());

        //rec_interpret_row_expr(&predicate.body, rowid, &mut interpreter)
        dri::interpret_row_fun(&predicate, arg_value)
    }
}

/*fn new_data_guide(tab_name: &str) -> Result<DataGuide, RuntimeError> {
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
}*/

/*fn cursor_data_guide(cursor: &Cursor) -> Result<DataGuide, RuntimeError> {
    match &cursor.cur_kind {
        CurKind::Read(cur_read) =>
            new_data_guide(&cur_read.tab_name),

        CurKind::Filter(filter_op) =>
            /* Filter: unchanged data guide (pass-through only) */
            cursor_data_guide(&filter_op.child_cursor),
    }
}*/

fn pull_filter(filter_node: &mut OpTree, block_mgr: &BlockMgr) -> Result<Option<usize>, RuntimeError> {
    /* For now, this code MUST be an anonymous function (no lowerings yet) */
    let pred_fun = match &filter_op.filter_code {
        ir::Expr::AnonFun(anon_fun) => &anon_fun,
        _ => return Err(RuntimeError::NotAFunction),
    };
    let child_cursor = &mut filter_op.child_cursor;
    //let data_guide = cursor_data_guide(child_cursor)?;
                    /* filter: data guide is unchanged (no map) */

    let mut child_row = next_row(child_cursor, block_mgr);
    let mut output_signal = None;
    /* FIXME we should do this only once */
    while let Ok(Some(row_val)) = child_row {
        match interpret_fun(pred_fun, row_val)? {
            RtVal::Bool(b) => {
                if b {
                    /* Filter passed, return rowid */
                    assert_eq!(BLOCK_SIZE, 1);
                    filter_op.output_fmt.write_val(row_val, block_mgr);
                    let curr_row_count = output_signal.unwrap_or(0);
                    output_signal = Some(curr_row_count + 1);
                }
                else {
                    /* Filter failed */
                    /* We have seen some rows, but filtered out */
                    output_signal = output_signal.or(Some(0));
                    /* Fetch the next row from the child, then loop */
                    child_row = next_row(child_cursor, block_mgr);
                }
            },

            val@_ => {
                return Err(
                        RuntimeError::FilterNotBooleanSci(val));
            }
        }
    }

    Ok(output_signal)
}

fn cursor_pull(cursor: &mut OpTree, block_mgr: &BlockMgr) -> Result<Option<usize>, RuntimeError> {
    match &mut cursor.row_op {
        RowOp::TableScan(cur_read) =>
            pull_ts(cur_read, block_mgr),

        RowOp::Filter(filter_op) =>
            pull_filter(&mut filter_op, block_mgr),
    }
}

/* FIXME this won't work with multi-row */
fn next_row(op_tree: &mut OpTree, block_mgr: &BlockMgr) -> Result<Option<RtVal>, RuntimeError> {
    while true {
        match cursor_pull(row_op, block_mgr)? {
            Some(1) => {
                let row_val = op_tree.output_fmt.build_val(block_mgr);
                return Ok(Some(row_val));
            }
            Some(0) => {
                /* rows were filtered out */
                /* try again */
            }
            Some(_) => {
                return Err(RuntimeError::MultiColNotSupported);
            }
            None => {
                return Ok(None)
            }
        }
    }
    unreachable!()
}

/*fn gen_proj_query_from_dataguide(data_guide: &DataGuide, rowid: Rowid) -> String {
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
}*/

/*fn query_sqlite_rowid_into(rowid: Rowid, data_guide: &DataGuide, res_buf: &mut [QVal])
    -> Result<(), RuntimeError>
{
    let sql_query = gen_proj_query_from_dataguide(data_guide, rowid);
    let _nrows = sqlexec::query_sqlite_into(&sql_query, res_buf)?;
    /* TODO assert that returned number of rows is exactly one */
    Ok(())
}*/

pub fn exec_interpreter_into(cursor: &mut Cursor, mut res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    //let data_guide = cursor_data_guide(cursor)?; /* TODO we should add the final "result conversion / out projection" node in the cursor tree */
    //let ncols = data_guide.0.len();
    let mut rowcount = 0;
    let block_mgr = BlockMgr::new(cursor.pipe_count);
    while let Some(rt_val) = next_row(&mut cursor.op_tree, &block_mgr)? {
        //let buf_idx = ncols * rowcount;
        //query_sqlite_rowid_into(rowid, &data_guide, &mut res_buf[buf_idx..])?;
        dri::write_rt_val_into(rt_val, &mut res_buf /* TODO this needs to move the slice forward */)?;
        rowcount += 1;
    }
    Ok(rowcount)
}
