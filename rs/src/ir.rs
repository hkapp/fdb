use std::fmt;

/* TODO remove */
#[derive(Debug)]
pub struct Decl {
    pub name: Global,
    pub body: Expr
}

/* Expr */

#[derive(Debug, Clone)]
pub enum Expr {
    AnonFun(AnonFun),
    FunCall(FunCall),
    LetExpr(LetExpr),
    PatMatch(PatMatch),
    LitVal(LitVal)
}

/* AnonFun */

#[derive(Debug, Clone)]
pub struct AnonFun {
    pub val_params: Vec<ValParam>,
    pub body:       Box<Expr>
}

/* ValParam */

#[derive(Debug, Clone)]
pub struct ValParam {
    pub name: Local,
}

/* FunCall */
/* Should we rename this? Most of the enum nodes will be operators actually.
 * Actual function call is going to be a special case.
 *
 * Action and Call look like very bad names though.
 * OpCall makes it sound not very functional.
 */

#[derive(Debug, Clone)]
pub struct FunCall {
    pub operator: Operator,
    pub val_args: Vec<ValArg>
}

#[derive(Debug, Clone)]
pub enum Operator {
    Noop,
    LessThanOrEqual,
    Plus,
    ReadRtCol(crate::fql::backend::dci::ColId), /* TODO re-design declarations for operator body */
    ReadRtColSci(crate::fql::backend::sci::ColId), /* TODO re-design declarations for operator body */
}

/* LetExpr */

#[derive(Debug, Clone)]
pub struct LetExpr {
    pub var_name:  Local,
    pub var_value: Box<Expr>,
    pub body:      Box<Expr>
}

/* PatMatch */

#[derive(Debug, Clone)]
pub struct PatMatch {
    pub matched_var: Local,
    pub pat_case:    PatCase, /* we only support structs, not enums right now */
}

/* PatCase */

#[derive(Debug, Clone)]
pub struct PatCase {
    pub constructor: Global,
    pub field_binds: Vec<Option<Local>>,  /* order is important here! */
    pub body:        Box<Expr>
}

/* LitVal */

#[derive(Debug, Clone)]
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
