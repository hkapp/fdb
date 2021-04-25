use crate::objstore::{self, ObjStore};

pub struct DbCtx {
    pub obj_store: ObjStore,
}

type Error = objstore::Error;

pub fn init_db() -> Result<DbCtx, Error> {
    let obj_store = objstore::load()?;

    let ctx =
        DbCtx {
            obj_store
        };
    Ok(ctx)
}
