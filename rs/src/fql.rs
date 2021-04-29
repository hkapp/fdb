use crate::objstore;
use crate::ctx::DbCtx;
use crate::objstore::Symbol;
use rusqlite as sqlite;

#[derive(Clone)]
pub enum QPlan {
    Read (String),
    Filter (Symbol, Box<QPlan>)
}

pub type QVal = u32;

#[derive(Debug)]
pub enum CompileError {
  SymbolNotDefined(Symbol),
  ObjectHasErrors(Symbol)
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
  CompileError(CompileError)
}

const DB_FILENAME: &str = "../data/fdb.db";

fn query_sqlite_into(query: &str, res_buf: &mut [QVal]) -> Result<usize, RuntimeError> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare(query)?;
    let mut rows = stmt.query([])?;
    let mut arr_pos = 0;

    while let Some(row) = rows.next()? {
        res_buf[arr_pos] = row.get(0)?;
        arr_pos += 1;
    }

    Ok(arr_pos)
}

const COLUMN_NAME: &str = "bar";

fn inline_filter_sql(fun_name: &Symbol, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    /* Retrieve the declaration */
    match db_ctx.obj_store.find(fun_name) {
        Some(obj) => {
            match obj.as_result() {
                /* TODO we should also check that the declaration at the end is actually a function
                 * and not a constant.
                 */
                Ok(_) => Ok(String::from("")),

                Err(objstore::FailedObj::ParseError(err_msg)) => {
                    println!("Object \"{}\" has parsing errors:", fun_name);
                    println!("{}", &err_msg);
                    return Err(
                        RuntimeError::CompileError(
                            CompileError::ObjectHasErrors(fun_name.clone())));
                }
            }
        }

        None =>
            return Err(
                RuntimeError::CompileError(
                    CompileError::SymbolNotDefined(fun_name.clone()))),
    }
}

fn rec_to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    use QPlan::*;
    let sql = match qplan {
        Read(tab_name) =>
            format!("SELECT {} FROM {}", COLUMN_NAME, tab_name),

        Filter(fun_name, rec_qplan) => {
            let rec_sql = rec_to_sql(&rec_qplan, db_ctx)?;
            let where_clause = inline_filter_sql(&fun_name, db_ctx)?;

            format!("SELECT {} FROM ({}) WHERE {}",
                    COLUMN_NAME, rec_sql, where_clause)
        },
    };
    Ok(sql)
}

fn to_sql(qplan: &QPlan, db_ctx: &DbCtx) -> Result<String, RuntimeError> {
    let mut sql = rec_to_sql(qplan, db_ctx)?;
    sql.push_str(";");
    Ok(sql)
}

/* TODO add enum codes like "HasMoreEntries" */
type Status = usize;

pub fn exec_into(qplan: &QPlan, db_ctx: &DbCtx, res_buf: &mut [QVal]) -> Result<Status, RuntimeError> {
    let sql_query = to_sql(qplan, db_ctx)?;
    println!("{}", sql_query);

    query_sqlite_into(&sql_query, res_buf)
}

/* Error conversion */

impl From<sqlite::Error> for RuntimeError {
    fn from(sql_err: sqlite::Error) -> Self {
        RuntimeError::SqliteError(sql_err)
    }
}
