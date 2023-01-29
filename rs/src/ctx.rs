use crate::objstore::{self, ObjStore};
use crate::fql::backend::Backend;

pub struct DbCtx {
    pub obj_store: ObjStore,
    pub backend:   Backend
}

type Error = objstore::Error;

pub fn init_db(backend: Backend) -> Result<DbCtx, Error> {
    let obj_store = objstore::load()?;

    let ctx =
        DbCtx {
            obj_store,
            backend
        };
    Ok(ctx)
}
