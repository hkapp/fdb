use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use crate::ghcdump;

pub struct ObjStore {
    symbols: HashMap<Symbol, Rc<Obj>>,
}

pub struct Obj {
    name: Symbol,
    body: Result<ValidObj, FailedObj>
}

pub type ValidObj = ghcdump::ir::Decl;

pub enum FailedObj {
    ParseError(ghcdump::ParseError)
}

#[derive(Debug)]
pub enum Error {
    ParsingError(ghcdump::FatalError),
}

/* Top-level loading */

pub fn load() -> Result<ObjStore, Error> {
    /* For now we're parsing the GHC dumps.
     * Also, the Object Store is exactly the parsing result (for now).
     */
    let ghc_decls =
        ghcdump::load_all()
            .map_err(|e| Error::ParsingError(e))?;

    let symbols =
        ghc_decls.into_iter()
            .map(|(global, parse_res)| {
                let sym = Symbol(global.into());
                let key = sym.clone();

                let obj_body = parse_res.map_err(|e| FailedObj::ParseError(e));
                let obj =
                    Obj {
                        name: sym,
                        body: obj_body
                    };
                let value = Rc::new(obj);

                (key, value)
            })
            .collect();

    let obj_store =
        ObjStore {
            symbols
        };
    Ok(obj_store)
}

/* Symbol */

#[derive(Hash, Eq, Clone, PartialEq, Debug)]
pub struct Symbol (String);

impl Symbol {
    pub fn new(name: String) -> Self {
        Symbol(name)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/* Obj API */

impl Obj {
    pub fn as_result(&self) -> Result<&ValidObj, &FailedObj> {
        self.body.as_ref()
    }

    pub fn obj_name(&self) -> &Symbol {
        &self.name
    }
}

/* ObjStore API */

impl ObjStore {
    fn insert_obj(&mut self, key: Symbol, value: Obj)
        -> Rc<Obj>
    {
        let new_obj = Rc::new(value);
        let ins_obj = Rc::clone(&new_obj);
        let key_cp = key.clone();

        let replaced = self.symbols.insert(key, ins_obj);

        match replaced {
            Some(prev_obj) => {
                match &prev_obj.body {
                    Ok(_decl) => {
                        println!("Replaced previous valid declaration for object {}",
                            prev_obj.name);
                    }
                    Err(_err) => {
                        println!("Replaced previous failed declaration for object {}",
                            prev_obj.name);
                    }
                }
                println!("Remaining references to evicted object: {}",
                    (Rc::strong_count(&prev_obj)) - 1);
            }

            None => {
                println!("Declared new object {}", key_cp);
            }
        }

        return new_obj;
    }

    #[allow(dead_code)]
    pub fn insert_valid(&mut self, key: Symbol, value: ValidObj)
        -> Rc<Obj>
    {
        let obj = Obj {
            name: key.clone(),
            body: Ok(value)
        };

        self.insert_obj(key, obj)
    }

    #[allow(dead_code)]
    pub fn insert_invalid(&mut self, key: Symbol, value: FailedObj)
        -> Rc<Obj>
    {
        let obj = Obj {
            name: key.clone(),
            body: Err(value)
        };

        self.insert_obj(key, obj)
    }

    pub fn find(&self, sym: &Symbol) -> Option<Rc<Obj>> {
        self.symbols.get(sym)
            .map(|o| Rc::clone(o))
    }
}
