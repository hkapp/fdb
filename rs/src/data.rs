use rusqlite as sqlite;

/* A bad data layer built directly on top of SQLite */

pub type DataError = sqlite::Error;

pub const DB_FILENAME: &str = "../data/fdb.db";
pub const STRUCT_COL_PREFIX: &str = "col";

pub fn sql_one_row_one_col<T>(query: &str) -> Result<T, DataError>
    where
        T: sqlite::types::FromSql
{
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare(&query)?;
    stmt.query_row([], |row| row.get(0))
        .map_err(Into::into)
}
