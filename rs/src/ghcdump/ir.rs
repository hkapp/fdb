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
    PatMatch(PatMatch),
    LitConv(LitConv)
}

/* AnonFun */

#[derive(Debug)]
pub struct AnonFun {
    pub type_params:      Vec<TypeParamF>,
    pub typeclass_params: Vec<TypeClassParam>,
    pub val_params:       Vec<ValParam>,
    pub body:             Box<Expr>  /* avoid recursive type */
}

/* TypeClassParam */

#[derive(Debug)]
pub struct TypeClassParam {
    pub name:      Local,
    pub typeclass: Type
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

/* PatMatch */

#[derive(Debug)]
pub struct PatMatch {
    pub matched_var: Local,
    pub pat_cases:   Vec<PatCase>,
}

/* PatCase */

#[derive(Debug)]
pub struct PatCase {
    pub constructor: Global,
    pub field_binds: Vec<Option<Local>>,  /* order is important here! */
    pub body:        Box<Expr>
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

/* Type */

#[derive(Debug)]
pub struct Type {
    pub name:      Global,
    pub type_args: Vec<Type>
}

/* Local */

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Local (pub String);

pub type TypeParamF = Local;
pub type ValArg = Local;

/* Global */

#[derive(Debug, Hash, Eq, Ord, PartialOrd, PartialEq, Clone)]
pub struct Global (pub String);

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
