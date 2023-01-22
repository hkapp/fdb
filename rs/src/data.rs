use rusqlite as sqlite;
use sqlite::Row;
use sqlite::types::FromSql;

/** A bad data layer built directly on top of SQLite */

pub type DataError = sqlite::Error;

pub const DB_FILENAME: &str = "../data/fdb.db";

pub fn sql_one_row_one_col<T>(query: &str) -> Result<T, DataError>
    where
        T: sqlite::types::FromSql
{
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let mut stmt = conn.prepare(&query)?;
    stmt.query_row([], |row| row.get(0))
        .map_err(Into::into)
}

pub type RowId = u64;

pub trait FromRow: Sized {
    fn from_row(row: &Row<'_>) -> Result<Self, DataError>;
}

impl<T: FromSql> FromRow for Vec<T> {
    fn from_row(row: &Row<'_>) -> Result<Self, DataError> {
        let mut vec = Vec::new();
        let mut i: usize = 0;
        loop {
            match row.get(i) {
                Ok(x)     => vec.push(x),
                Err(InvalidColumnIndex) => return Ok(vec),
                Err(err@_) => return Err(err),
            }
            i += 1;
        }
    }
}

pub fn table_row<T: FromRow>(tab_name: &str, rowid: RowId) -> Result<T, DataError> {
    let conn = sqlite::Connection::open(DB_FILENAME)?;

    let query = format!("SELECT * FROM {} WHERE rowid={}", tab_name, rowid);
    let mut stmt = conn.prepare(&query)?;
    stmt.query_row([], |row| FromRow::from_row(row))
}
