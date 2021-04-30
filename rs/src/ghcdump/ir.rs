use std::fmt;

#[derive(Debug)]
pub struct Decl {
    pub name: Global,
    pub body: Expr
}

/* Expr */

/* TODO consider type Expr = Box<Expr_> */
#[derive(Debug)]
pub enum Expr {
    AnonFun(AnonFun),
    FunCall(FunCall),
    LetExpr(LetExpr),
    LitConv(LitConv)
}

/* AnonFun */

#[derive(Debug)]
pub struct AnonFun {
    pub type_params: Vec<TypeParamF>,
    pub val_params:  Vec<ValParam>,
    pub body:        Box<Expr>  /* avoid recursive type */
}

/* ValParam */

#[derive(Debug)]
pub struct ValParam {
    pub name: Local,
    pub typ:  Type
}

/* FunCall */

#[derive(Debug)]
pub struct FunCall {
    pub called_fun:     Global,
    pub type_args:      Vec<TypeArg>,
    pub typeclass_args: Vec<TypeClassArg>,
    pub val_args:       Vec<ValArg>
}

/* LetExpr */

#[derive(Debug)]
pub struct LetExpr {
    pub var_name:  Local,
    pub var_type:  Type,
    pub var_value: Box<Expr>,
    pub body:      Box<Expr>
}

/* LitConv */

#[derive(Debug)]
pub struct LitConv {
    pub conv_fun: Global,
    pub raw_lit:  RawLit
}

#[derive(Debug)]
pub enum RawLit {
    IntLit(i32)
}

/* Local */

#[derive(Debug, Eq, PartialEq)]
pub struct Local (pub String);

pub type TypeParamF = Local;
pub type ValArg = Local;

/* Global */

#[derive(Debug, Hash, Eq, Ord, PartialOrd, PartialEq, Clone)]
pub struct Global (pub String);

pub type Type = Global;
pub type TypeClassArg = Global;

pub type TypeArg = Type;

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