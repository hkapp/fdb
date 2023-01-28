use lazy_static::lazy_static;

pub struct Table {
    pub columns: Vec<String> /* TODO add type information */
}

lazy_static! {
    pub static ref TABLE_FOO:   Table = Table { columns: vec!["bar".to_string()] };
    pub static ref TABLE_PAIRS: Table = Table { columns: vec!["col0".to_string(), "col1".to_string()] };
}

pub fn resolve(tab_name: &str) -> Option<&'static Table> {
    match tab_name {
        "foo"   => Some(&TABLE_FOO),
        "pairs" => Some(&TABLE_PAIRS),
        _       => None,
    }
}
