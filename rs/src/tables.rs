
pub struct Table {
    pub columns: Vec<String> /* TODO add type information */
}

pub const TABLE_FOO:   Table = Table { columns: vec!["bar".to_string()] };

//pub const STRUCT_COL_PREFIX: &str = "col";
pub const TABLE_PAIRS: Table = Table { columns: vec!["col0".to_string(), "col1".to_string()] };

pub fn resolve(tab_name: &str) -> Option<&'static Table> {
    match tab_name {
        "foo"   => Some(&TABLE_FOO),
        "pairs" => Some(&TABLE_PAIRS),
        _       => None,
    }
}
