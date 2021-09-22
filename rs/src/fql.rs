use crate::objstore;
use crate::ctx::DbCtx;
use crate::objstore::Symbol;
use rusqlite as sqlite;
use crate::ghcdump::ir;
use std::collections::HashMap;
use std::ops;
use std::rc::Rc;

#[derive(Clone)]
pub enum QPlan {
    Read (String),
    Filter (Symbol, Box<QPlan>)
}

pub type QVal = u32;

#[derive(Debug)]
pub enum CompileError {
  SymbolNotDefined(Symbol),
  ObjectHasErrors(Symbol),
  NotAFunction { symbol: Symbol, resolves_to: String },
}

/* QUERY CONSTRUCTION */

pub fn filter(prev_plan: &QPlan, fun_name: &str, db_ctx: &DbCtx) -> Result<QPlan, CompileError> {
    let symbol =
        objstore::Symbol::new(
            String::from(fun_name));

    match db_ctx.obj_store.find(&symbol) {
        Some(obj) => {
            match obj.as_result() {
                /* TODO we should also check that the declaration at the end is actually a function
                 * and not a constant.
                 */
                Ok(_) => (),

                Err(objstore::FailedObj::ParseError(err_msg)) => {
                    println!("Object \"{}\" has parsing errors:", fun_name);
                    println!("{}", &err_msg);
                    return Err(
                        CompileError::ObjectHasErrors(symbol));
                }
            }
        }

        None =>
            return Err(
                CompileError::SymbolNotDefined(symbol)),
    }

    let prev_plan_cp = Box::new(prev_plan.clone());
    let new_plan = QPlan::Filter(symbol, prev_plan_cp);
                        /* need to clone to ensure Haskell doesn't ask
                         * the same memory to be cleaned twice */

    Ok(new_plan)
}

/* RUNTIME */

#[derive(Debug)]
pub enum RuntimeError {
  SqliteError(sqlite::Error),
  CompileError(CompileError),
  TooManyArguments(usize),
  UnsupportedFunction(ir::Global),
  ConflictingDefForVar(ir::Local),
  TooManyCases(usize),
  BufferTooSmall(usize),
  IndexNotInDataGuide(usize),
  UnsupportedExpression(String),
  UndefinedVariable(ir::Local),
  PatternMatchNonStruct(ir::Local),
  UnsupportedComparison { left: RtVal, right: RtVal },
  FilterNotBoolean(RtVal),
}

const DB_FILENAME: &str = "../data/fdb.db";

fn query_sqlite_into(query: &str, res_buf: &mut [QVal]) -> Result<usize, RuntimeError> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

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

/* Object store helpers */

fn resolve_symbol(symbol: &Symbol, db_ctx: &DbCtx) -> Result<Rc<objstore::Obj>, CompileError> {
    /* Retrieve the declaration */
    db_ctx.obj_store.find(symbol)
        .ok_or_else(|| CompileError::SymbolNotDefined(symbol.clone()))
}

fn extract_decl(obj: &objstore::Obj) -> Result<&ir::Decl, CompileError> {
    obj.as_result()
        .map_err(|e|
            match e {
                objstore::FailedObj::ParseError(err_msg) => {
                    let symbol = obj.obj_name();
                    println!("Object \"{}\" has parsing errors:", symbol);
                    println!("{}", &err_msg);
                    CompileError::ObjectHasErrors(symbol.clone())
                }
            }
        )
}

fn check_is_fun_decl(decl: &ir::Decl) -> Result<&ir::AnonFun, CompileError> {
    use ir::Expr::*;
    match &decl.body {
        /* This is the only accepted case */
        AnonFun(anon_fun) => Ok(&anon_fun),

        /* Every other case is an error */
        _ => {
            /* Gather exactly what it was for the error message */
            let what = match &decl.body {
                FunCall(_)  => "Function call",
                LetExpr(_)  => "Let expression",
                PatMatch(_) => "Pattern matching",
                LitConv(_)  => "Literal conversion",

                AnonFun(_)  => unreachable!(),
            };
            let fun_name = Symbol::new(decl.name.0.clone());

            Err(
                CompileError::NotAFunction {
                    symbol:      fun_name,
                    resolves_to: String::from(what)
                })
        }
    }
}

const STRUCT_COL_PREFIX: &str = "col";

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
                let sql_col = format!("{}{}", STRUCT_COL_PREFIX, field_index);
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
    let fun_obj = resolve_symbol(fun_name, db_ctx)?;
    let fun_decl = extract_decl(&fun_obj)?;
    check_is_fun_decl(fun_decl)?;

    let mut eval_state = HashMap::default();
    rec_inline_filter_sql(&fun_decl.body, &mut eval_state)
}

fn rec_to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    use QPlan::*;
    let sql = match qplan {
        Read(tab_name) =>
            format!("SELECT * FROM {}", tab_name),

        Filter(fun_name, rec_qplan) => {
            let rec_sql = rec_to_sql(&rec_qplan, db_ctx)?;
            let where_clause = inline_filter_sql(&fun_name, db_ctx)?;

            format!("SELECT * FROM ({}) WHERE {}",
                    rec_sql, where_clause)
        },
    };
    Ok(sql)
}

fn to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    let mut sql = rec_to_sql(qplan, db_ctx)?;
    sql.push_str(";");
    Ok(sql)
}

/* Interpreter: preparation step */

enum Cursor {
    Read(CurRead),
    Filter(CurFilter)
}

struct CurRead {
    rowid_range: ops::Range<Rowid>,
    tab_name:    String,
}

struct CurFilter {
    pred_obj:     Rc<objstore::Obj>,
    child_cursor: Box<Cursor>,
}

type Rowid = u32;

fn sql_one_row_one_col<T>(query: &str) -> Result<T, RuntimeError>
    where
        T: sqlite::types::FromSql
{
    let conn = sqlite::Connection::open(DB_FILENAME)?;

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
        CurRead {
            rowid_range,
            tab_name: String::from(tab_name)
        };

    let cursor = Cursor::Read(cur_read);
    Ok(cursor)
}

fn filter_cursor(fun_name: &Symbol, child_qplan: &QPlan, db_ctx: &DbCtx)
    -> Result<Cursor, RuntimeError>
{
    let child_cursor = to_cursor(&child_qplan, db_ctx)?;

    let pred_obj = {
        let fun_obj = resolve_symbol(fun_name, db_ctx)?;
        let fun_decl = extract_decl(&fun_obj)?;
        check_is_fun_decl(fun_decl)?;

        fun_obj
    };

    let cur_filter =
        CurFilter {
            pred_obj,
            child_cursor: Box::new(child_cursor)
        };

    let cursor = Cursor::Filter(cur_filter);
    Ok(cursor)
}

fn to_cursor(qplan: &QPlan, db_ctx: &DbCtx) -> Result<Cursor, RuntimeError> {
    use QPlan::*;
    match qplan {
        Read(tab_name) =>
            read_cursor(&tab_name),

        Filter(fun_name, child_qplan) =>
            filter_cursor(&fun_name, &child_qplan, db_ctx),
    }
}

/* Interpreter: execution step */

#[derive(Debug, Clone)]
struct ColId {
    col_name: String,
    tab_name: String
}

#[derive(Debug, Clone)]
struct DataGuide(Vec<ColId>);

#[derive(Debug, Clone)]
enum RtVal {
    UInt32(u32),
    Bool(bool),
    DataGuide(DataGuide)
}

type Interpreter<'a> = HashMap<&'a ir::Local, RtVal>;

fn cursor_fetch_read(cur_read: &mut CurRead) -> Result<Option<Rowid>, RuntimeError> {
    Ok(cur_read.rowid_range.next())
}

fn resolve_dataguide_entry(data_guide: &DataGuide, field_index: usize, rowid: Rowid)
    -> Result<RtVal, RuntimeError>
{
    let column = data_guide.0.get(field_index)
                    .ok_or_else(|| RuntimeError::IndexNotInDataGuide(field_index))?;

    let query = format!("SELECT {} FROM {} WHERE ROWID = {}",
                        column.col_name, column.tab_name, rowid);
    let col_val = sql_one_row_one_col(&query)?;
    let rt_val = RtVal::UInt32(col_val);

    Ok(rt_val)
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

        PatMatch(ir::PatMatch { matched_var, pat_cases }) => {
            /* FIXME might need to pop off state here if the let expression
             * also declares variables whose name conflict with existing ones.
             */
            if pat_cases.len() > 1 {
                /* We don't support actual ADTs right now in the interpreter
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

                let matched_val = interpreter.get(&matched_var)
                                    .ok_or_else(|| RuntimeError::UndefinedVariable(
                                                        matched_var.clone()))?;
                let data_guide = match &matched_val {
                    RtVal::DataGuide(dg) => dg,
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

        FunCall(ir::FunCall { called_fun, val_args, .. }) => {
            /* Do we know this function? */
            match &called_fun.0[..] {
                "GHC.Num.fromInteger" => {
                    assert!(val_args.len() == 1);
                    let arg_name = val_args.get(0).unwrap();
                    let arg_val = interpreter.get(arg_name).unwrap();
                    Ok(arg_val.clone())
                }

                "GHC.Classes.<=" => {
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
                                RuntimeError::UnsupportedComparison{
                                    left:  val_left.clone(),
                                    right: val_right.clone()
                                }),
                    }
                }

                _ =>
                    Err(
                        RuntimeError::UnsupportedFunction(called_fun.clone())),
            }
        }

        LitConv(ir::LitConv { raw_lit, .. }) => {
            match raw_lit {
                ir::RawLit::IntLit(n) =>
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
        let conflict = interpreter.insert(param_name,
                                          RtVal::DataGuide(data_guide.clone()));
        assert!(conflict.is_none());

        rec_interpret_row_expr(&predicate.body, rowid, &mut interpreter)
    }
}

fn new_data_guide(tab_name: &str) -> DataGuide {
    let ncols = 2; /* for now this is the max */
    let mut cols = Vec::new();

    for col_idx in 0..ncols {
        let col_name = format!("{}{}", STRUCT_COL_PREFIX, col_idx);
        let column =
            ColId {
                col_name,
                tab_name: String::from(tab_name)
            };

        cols.push(column)
    }

    DataGuide(cols)
}

fn cursor_data_guide(cursor: &Cursor) -> DataGuide {
    match cursor {
        Cursor::Read(cur_read) =>
            new_data_guide(&cur_read.tab_name),

        Cursor::Filter(cur_filter) =>
            /* Filter: unchanged data guide (pass-through only) */
            cursor_data_guide(&cur_filter.child_cursor),
    }
}

fn cursor_fetch_filter(cur_filter: &mut CurFilter) -> Result<Option<Rowid>, RuntimeError> {
    let pred_decl = extract_decl(&cur_filter.pred_obj)?;
    let pred_fun = check_is_fun_decl(pred_decl)?;
    let child_cursor = &mut cur_filter.child_cursor;
    let data_guide = cursor_data_guide(child_cursor);
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
                        RuntimeError::FilterNotBoolean(val));
            }
        }
    }

    /* The child row was either Err(_) or Ok(None) */
    /* Return that row in either case */
    return child_row;
}

fn cursor_fetch(cursor: &mut Cursor) -> Result<Option<Rowid>, RuntimeError> {
    match cursor {
        Cursor::Read(cur_read) =>
            cursor_fetch_read(cur_read),

        Cursor::Filter(cur_filter) =>
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
    let _nrows = query_sqlite_into(&sql_query, res_buf)?;
    /* TODO assert that returned number of rows is exactly one */
    Ok(())
}

fn exec_interpreter_into(cursor: &mut Cursor, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let data_guide = cursor_data_guide(cursor); /* TODO we should add the final "result conversion / out projection" node in the cursor tree */
    let mut rowcount = 0;
    while let Some(rowid) = cursor_fetch(cursor)? {
        /* TODO: read row values from rowid
         *   base code off of query_sqlite_into?
         */
        let buf_idx = 2 * rowcount; /* currently, read pairs of ints from the table */
        query_sqlite_rowid_into(rowid, &data_guide, &mut res_buf[buf_idx..])?;
        rowcount += 1;
    }
    Ok(rowcount)
}

/* TODO add enum codes like "HasMoreEntries" */
type Status = usize;

pub fn exec_into(qplan: &QPlan, db_ctx: &DbCtx, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let sqlite_backend = false;

    if sqlite_backend {
        /* Execute on full SQLite backend */
        let sql_query = to_sql(qplan, db_ctx)?;

        println!("SQLite execution:");
        println!("{}", sql_query);

        query_sqlite_into(&sql_query, res_buf)
    }
    else {
        /* Execute on current interpreter backend */
        let mut cursor = to_cursor(qplan, db_ctx)?;

        println!("FDB interpreter:");
        exec_interpreter_into(&mut cursor, res_buf)
    }
}

/* Error conversion */

impl From<sqlite::Error> for RuntimeError {
    fn from(sql_err: sqlite::Error) -> Self {
        RuntimeError::SqliteError(sql_err)
    }
}

impl From<CompileError> for RuntimeError {
    fn from(err: CompileError) -> Self {
        RuntimeError::CompileError(err)
    }
}
