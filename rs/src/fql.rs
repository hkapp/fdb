use crate::objstore;
use crate::ctx::DbCtx;

#[derive(Clone)]
pub enum QPlan {
    Read (String),
    Filter (objstore::Symbol, Box<QPlan>)
}

type Error = ();

pub fn filter(prev_plan: &QPlan, fun_name: &str, db_ctx: &DbCtx) -> Result<QPlan, Error> {
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
                    /* TODO turn panic into Err(_) */
                    panic!("Object \"{}\" had parsing errors", fun_name)
                }
            }
        }

        None =>
            /* TODO turn panic into Err(_) */
            panic!("Filter function \"{}\" was never defined", fun_name),
    }

    let prev_plan_cp = Box::new(prev_plan.clone());
    let new_plan = QPlan::Filter(symbol, prev_plan_cp);
                        /* need to clone to ensure Haskell doesn't ask
                         * the same memory to be cleaned twice */

    Ok(new_plan)
}
