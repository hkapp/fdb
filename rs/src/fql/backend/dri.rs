use crate::ir;
use crate::fql;
use crate::fql::{QPlan, SQPlan, RuntimeError, QVal, Status};
use crate::ctx::DbCtx;
use std::ops;
use crate::objstore;
use rusqlite as sqlite;
use std::rc::Rc;
use std::collections::HashMap;
use super::BufWriter;
use crate::tables::TABLE_PAIRS;

/* Interpreter: preparation step */

pub enum Cursor {
    ReadT(CurReadT),
    Filter(CurFilter),
    Map(CurMap),
}

pub struct CurReadT {
    rowid_range: ops::Range<Rowid>,
    tab_name:    String,
    data_guide:  DataGuide /* TODO rename */
}

pub struct CurFilter {
    pred_obj:     Rc<objstore::Obj>,
    child_cursor: Box<Cursor>,
}

pub struct CurMap {
    mapfun_obj:   Rc<objstore::Obj>,
    child_cursor: Box<Cursor>,
}

pub enum SQCursor {
    Fold(CurFold)
}

pub struct CurFold {
    foldfun_obj:  Rc<objstore::Obj>,
    zerofun_obj:  Rc<objstore::Obj>,
    child_cursor: Box<Cursor>,
}

pub type Rowid = u32;

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
        CurReadT {
            rowid_range,
            tab_name:   String::from(tab_name),
            data_guide: new_data_guide(tab_name)?
        };

    let cursor = Cursor::ReadT(cur_read);
    Ok(cursor)
}

fn filter_cursor(filter_fun: Rc<objstore::Obj>, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = to_cursor(&child_qplan, db_ctx)?;

    let fun_decl = super::extract_decl(&filter_fun)?;
    super::check_is_fun_decl(fun_decl)?;

    let cur_filter =
        CurFilter {
            pred_obj:     filter_fun,
            child_cursor: Box::new(child_cursor)
        };

    let cursor = Cursor::Filter(cur_filter);
    Ok(cursor)
}

fn map_cursor(map_fun: Rc<objstore::Obj>, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = to_cursor(&child_qplan, db_ctx)?;

    let fun_decl = super::extract_decl(&map_fun)?;
    super::check_is_fun_decl(fun_decl)?;

    let cur_map =
        CurMap {
            mapfun_obj:   map_fun,
            child_cursor: Box::new(child_cursor)
        };

    let cursor = Cursor::Map(cur_map);
    Ok(cursor)
}

pub fn to_cursor(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    use QPlan::*;
    match qplan {
        ReadT(qreadt) =>
            read_cursor(&qreadt.tab_name),

        Filter(qfilter) =>
            filter_cursor(Rc::clone(&qfilter.filter_fun), &qfilter.qchild, db_ctx),

        Map(qmap) =>
            map_cursor(Rc::clone(&qmap.map_fun), &qmap.qchild, db_ctx),
    }
}

fn fold_sqcursor(sqplan: &fql::SQFold, db_ctx: &DbCtx)
    -> Result<SQCursor, RuntimeError>
{
    let child_cursor = to_cursor(&sqplan.qchild, db_ctx)?;

    let fold_fun = &sqplan.fold_fun;
    let fun_decl = super::extract_decl(fold_fun)?;
    super::check_is_fun_decl(fun_decl)?;

    let zero_fun = &sqplan.zero_fun;
    //let zero_decl = super::extract_decl(zero_fun)?;
    //super::check_is_fun_decl(zero_decl)?;

    let cursor =
        SQCursor::Fold(
            CurFold {
                foldfun_obj:  Rc::clone(fold_fun),
                zerofun_obj:  Rc::clone(zero_fun),
                child_cursor: Box::new(child_cursor)
            }
        );

    Ok(cursor)
}

pub fn to_sqcursor(sqplan: &SQPlan, db_ctx: &DbCtx) -> Result<SQCursor, RuntimeError> {
    use SQPlan::*;
    match sqplan {
        Fold(fold) =>
            fold_sqcursor(&fold, db_ctx),
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

#[derive(Debug, Clone)]
pub enum RtVal {
    UInt32(u32),
    Bool(bool),
    Struct(RtStruct),
}

#[derive(Debug, Clone)]
pub struct RtStruct {
    pub fields: Vec<RtVal>
}

struct Interpreter<'a> {
    var_state: HashMap<&'a ir::Local, RtVal>
}

impl<'a> Interpreter<'a> {
    fn new() -> Self {
        Interpreter {
            var_state: HashMap::new(),
        }
    }

    fn read_var(&self, var_name: &ir::Local) -> Result<&RtVal, RuntimeError> {
        self.var_state
            .get(var_name)
            .ok_or_else(||
                RuntimeError::UndefinedVariable(var_name.clone()))
    }

    fn let_var(&mut self, var_name: &'a ir::Local, var_value: RtVal) -> Result<(), RuntimeError> {
        let conflict = self.var_state.insert(var_name, var_value);

        if conflict.is_some() {
            Err(RuntimeError::ConflictingDefForVar(var_name.clone()))
        }
        else {
            Ok(())
        }
    }
}

/* TODO rename DataGuide if only for this use */
fn read_row(tab_name: &str, rowid: Rowid, format: &DataGuide) -> Result<RtVal, RuntimeError>
{
    let sql_query = gen_proj_query_from_dataguide(format, rowid);

    let conn = sqlite::Connection::open(super::DB_FILENAME)?;
    let mut stmt = conn.prepare(&sql_query)?;
    let mut sql_rows = stmt.query([])?;

    let col_count = sql_rows.column_count().unwrap();

    /* Get the first row */
    let row =
        match sql_rows.next()? {
            Some(row) => row,

            None => {
                /* There should be exactly one row
                 * This is an error
                 */
                return Err(RuntimeError::NoRowWithRowid(String::from(tab_name), rowid));
            }
        };

    /* Convert the column values to RtVal */
    /* TODO can we do this process with a map on an iterator
     *  over the columns? (+ collect in the end)
     */
    let mut rt_cols = Vec::new(); /* TODO pre allocate to col_count */
    for col_idx in 0..col_count {
        /* For now, we're always assuming columns of type i32 */
        let rt_val = RtVal::UInt32(row.get(col_idx)?);
        rt_cols.push(rt_val);
    }

    let row_val =
        if rt_cols.len() == 1 {
            rt_cols.pop().unwrap()
        }
        else {
            RtVal::Struct(
                RtStruct {
                    fields: rt_cols
                }
            )
        };

    /* Make sure that there is only one row */
    if sql_rows.next()?.is_some() {
        return Err(RuntimeError::TooManyRowsWithRowid(String::from(tab_name), rowid));
    }

    Ok(row_val)
}

fn cursor_fetch_read(cur_read: &mut CurReadT) -> Result<Option<RtVal>, RuntimeError> {
    match cur_read.rowid_range.next() {
        Some(rowid) => {
            let rt_val = read_row(&cur_read.tab_name, rowid, &cur_read.data_guide)?;
            Ok(Some(rt_val))
        }

        None =>
            Ok(None),
    }
}

fn rec_interpret_row_expr<'a>(expr: &'a ir::Expr, interpreter: &mut Interpreter<'a>)
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
            let rt_val = rec_interpret_row_expr(var_value, interpreter)?;

            interpreter.let_var(var_name, rt_val)?;

            rec_interpret_row_expr(body, interpreter)
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
                /* TODO: use some 'zip_with_index' iterator */
                let field_bind = field_binds.get(field_index).unwrap();

                if field_bind.is_none() {
                    /* this index is not bound */
                    continue;
                }
                let field_bind = field_bind.as_ref().unwrap();

                let matched_val = interpreter.read_var(&matched_var)?;
                let rt_struct = match &matched_val { /* TODO move outside the loop */
                    RtVal::Struct(s) => s,
                    _ => return Err(RuntimeError::PatternMatchNonStruct(matched_var.clone())),
                };

                let field_val = rt_struct.fields.get(field_index).unwrap().clone();
                interpreter.let_var(&field_bind, field_val)?;
            }

            rec_interpret_row_expr(&deconstruct.body, interpreter)
        }

        FunCall(ir::FunCall { operator, val_args }) => {
            use ir::Operator;
            match &operator {
                Operator::Noop => {
                    assert!(val_args.len() == 1);
                    let arg_name = val_args.get(0).unwrap();
                    let arg_val = interpreter.read_var(arg_name)?;
                    Ok(arg_val.clone())
                }

                Operator::LessThanOrEqual => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let val_left  = interpreter.read_var(arg_left)?;
                    let val_right = interpreter.read_var(arg_right)?;

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

                Operator::Plus => {
                    assert!(val_args.len() == 2);
                    let arg_left  = val_args.get(0).unwrap();
                    let arg_right = val_args.get(1).unwrap();

                    let val_left  = interpreter.read_var(arg_left)?;
                    let val_right = interpreter.read_var(arg_right)?;

                    match (val_left, val_right) {
                        (RtVal::UInt32(int_left), RtVal::UInt32(int_right)) =>
                            Ok(
                                RtVal::UInt32(int_left + int_right)),

                        _ =>
                            Err(
                                RuntimeError::UnsupportedAddition{
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

fn interpret_expr(expr: &ir::Expr) -> Result<RtVal, RuntimeError> {
    let mut interpreter = Interpreter::new();
    rec_interpret_row_expr(&expr, &mut interpreter)
}

fn interpret_fun(function: &ir::AnonFun, arg_values: Vec<RtVal>)
    -> Result<RtVal, RuntimeError>
{
    let val_params = &function.val_params;
    if val_params.len() != arg_values.len() {
        /* TODO change the error message */
        Err(RuntimeError::TooManyArguments(val_params.len()))
    }
    else {
        let mut interpreter = Interpreter::new();

        for (param, arg_value) in std::iter::zip(val_params.iter(),
                                                 arg_values.into_iter())
        {
            let param_name: &ir::Local = &param.name;
            interpreter.let_var(param_name, arg_value)?;
        }

        rec_interpret_row_expr(&function.body, &mut interpreter)
    }
}

/* TODO rename */
// Public because it is used in sci
pub fn interpret_row_fun(function: &ir::AnonFun, arg_value: RtVal)
    -> Result<RtVal, RuntimeError>
{
    let arg_values = vec![arg_value];
    interpret_fun(function, arg_values)
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
        let cols = TABLE_PAIRS.columns
                        .iter()
                        .map(|col_name| ColId {
                            col_name: col_name.clone(),
                            tab_name: String::from(tab_name)
                        })
                        .collect();

        DataGuide(cols)
    }

    fn dg_lineitem_v1(tab_name: &str) -> DataGuide {
        /* See create_lineitem_v1.sql */
        let col_names = vec!["l_quantity", "l_extendedprice",
                             "l_discount", "l_shipdate"];
        let mut cols = Vec::new();

        for col_name in col_names {
            let column =
                ColId {
                    col_name: String::from(col_name),
                    tab_name: String::from(tab_name)
                };

            cols.push(column)
        }

        DataGuide(cols)
    }

    /* FIXME for now the table schemas are hardcoded */
    match tab_name {
      "foo"         => Ok(dg_foo(tab_name)),
      "pairs"       => Ok(dg_pairs(tab_name)),
      "lineitem_v1" => Ok(dg_lineitem_v1(tab_name)),
      _             => Err(RuntimeError::UnknownTable(String::from(tab_name))),
    }
}

fn cursor_fetch_filter(cur_filter: &mut CurFilter) -> Result<Option<RtVal>, RuntimeError> {
    let pred_decl = super::extract_decl(&cur_filter.pred_obj)?;
    let pred_fun = super::check_is_fun_decl(pred_decl)?;
    let child_cursor = &mut cur_filter.child_cursor;

    let mut child_res = cursor_fetch(child_cursor);
    while let Ok(Some(child_row)) = child_res {
        match interpret_row_fun(pred_fun, child_row.clone())? {
            RtVal::Bool(b) => {
                if b {
                    /* Filter passed, return rowid */
                    return Ok(Some(child_row));
                }
                else {
                    /* Filter failed */
                    /* Fetch the next row from the child, then loop */
                    child_res = cursor_fetch(child_cursor);
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
    return child_res;
}

fn cursor_fetch_map(cur_map: &mut CurMap) -> Result<Option<RtVal>, RuntimeError> {
    let child_cursor = &mut cur_map.child_cursor;
    let child_res = cursor_fetch(child_cursor)?;

    if child_res.is_none() {
        /* no more rows */
        return Ok(None);
    }
    let child_val = child_res.unwrap();

    let mapfun_decl = super::extract_decl(&cur_map.mapfun_obj)?;
    let map_fun = super::check_is_fun_decl(mapfun_decl)?;

    let map_res = interpret_row_fun(map_fun, child_val)?;
    Ok(Some(map_res))
}

fn cursor_fetch(cursor: &mut Cursor) -> Result<Option<RtVal>, RuntimeError> {
    match cursor {
        Cursor::ReadT(cur_read) =>
            cursor_fetch_read(cur_read),

        Cursor::Filter(cur_filter) =>
            cursor_fetch_filter(cur_filter),

        Cursor::Map(cur_map) =>
            cursor_fetch_map(cur_map),
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

pub fn write_rtval_to_buffer<'a>(row_val: RtVal, output: &mut BufWriter<'a, QVal>) -> Result<(), RuntimeError> {
    match row_val {
        RtVal::Struct( RtStruct { fields } ) => {
            for field_val in fields {
                write_rtval_to_buffer(field_val, output)?;
            }
        },

        RtVal::UInt32(int_val) =>
            output.push(int_val),

        _ => {
            return Err(RuntimeError::CantWriteRtValToBuffer(row_val));
        }
    };

    Ok(())
}

pub fn exec_interpreter_into(cursor: &mut Cursor, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let mut rowcount = 0;
    let mut buf_writer = BufWriter::new(res_buf);
    while let Some(row_val) = cursor_fetch(cursor)? {
        /* TODO: use u8 buffers, and let the 'write' function tell
         *  what is the buffer position now.
         */
        write_rtval_to_buffer(row_val, &mut buf_writer)?;
        rowcount += 1;
    }
    Ok(rowcount)
}

fn sqcursor_exec_fold(cur_fold: &mut CurFold) -> Result<RtVal, RuntimeError> {
    let zerofun_decl = super::extract_decl(&cur_fold.zerofun_obj)?;
    //let zero_fun = super::check_is_fun_decl(zerofun_decl)?;

    let mut curr_val = interpret_expr(&zerofun_decl.body)?;

    let foldfun_decl = super::extract_decl(&cur_fold.foldfun_obj)?;
    let fold_fun = super::check_is_fun_decl(foldfun_decl)?;

    let child_cursor = &mut cur_fold.child_cursor;
    while let Some(child_res) = cursor_fetch(child_cursor)? {
        let fold_args = vec![curr_val, child_res];
        curr_val = interpret_fun(fold_fun, fold_args)?;
    }

    Ok(curr_val)
}

fn sqcursor_exec(cursor: &mut SQCursor) -> Result<RtVal, RuntimeError> {
    match cursor {
        SQCursor::Fold(cur_fold) =>
            sqcursor_exec_fold(cur_fold),
    }
}

pub fn execsq_interpreter_into(cursor: &mut SQCursor, res_buf: &mut QVal) -> Result<bool, RuntimeError> {
    let cursor_res = sqcursor_exec(cursor)?;
    match cursor_res {
        RtVal::UInt32(int_val) => {
            *res_buf = int_val;
            Ok(true)
        }
        _ => {
            Err(RuntimeError::CantWriteRtValToBuffer(cursor_res))
        }
    }
}
