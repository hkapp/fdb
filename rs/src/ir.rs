use std::fmt;

#[derive(Debug)]
pub struct Decl {
    pub name: Global,
    pub body: Expr
}

/* Expr */

#[derive(Debug)]
pub enum Expr {
    AnonFun(AnonFun),
    FunCall(FunCall),
    LetExpr(LetExpr),
    PatMatch(PatMatch),
    LitVal(LitVal)
}

/* AnonFun */

#[derive(Debug)]
pub struct AnonFun {
    pub val_params: Vec<ValParam>,
    pub body:       Box<Expr>
}

/* ValParam */

#[derive(Debug)]
pub struct ValParam {
    pub name: Local,
}

/* FunCall */
/* TODO temp replace by operators */

#[derive(Debug)]
pub struct FunCall {
    pub called_fun: Global,
    pub val_args:   Vec<ValArg>
}

/* LetExpr */

#[derive(Debug)]
pub struct LetExpr {
    pub var_name:  Local,
    pub var_value: Box<Expr>,
    pub body:      Box<Expr>
}

/* PatMatch */

#[derive(Debug)]
pub struct PatMatch {
    pub matched_var: Local,
    pub pat_case:    PatCase, /* we only support structs, not enums right now */
}

/* PatCase */

#[derive(Debug)]
pub struct PatCase {
    pub constructor: Global,
    pub field_binds: Vec<Option<Local>>,  /* order is important here! */
    pub body:        Box<Expr>
}

/* LitVal */

#[derive(Debug)]
pub enum LitVal {
    IntLit(i32)
}

/* Local */

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Local (pub String);

pub type ValArg = Local;

/* Global */

#[derive(Debug, Hash, Eq, Ord, PartialOrd, PartialEq, Clone)]
pub struct Global (pub String);

impl fmt::Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Into<String> for Global {
    fn into(self) -> String {
        self.0
    }
}
