use crate::ghcdump::ir as ghcir;
use crate::ir as cmnir;

#[derive(Debug)]
pub enum ConvError {
    TooManyPatCases(usize),
    UnknownOperator(ghcir::Global),
}

/* Decl */
pub fn conv_decl(ghc_decl: &ghcir::Decl) -> Result<cmnir::Decl, ConvError> {
    let conv_decl = cmnir::Decl {
        name: conv_global(&ghc_decl.name),
        body: conv_expr(&ghc_decl.body)?,
    };
    Ok(conv_decl)
}

/* Expr */
fn conv_expr(ghc_expr: &ghcir::Expr) -> Result<cmnir::Expr, ConvError> {
    use ghcir::Expr as GhcExpr;
    use cmnir::Expr as CmnExpr;

    let conv_expr =
        match ghc_expr {
            GhcExpr::AnonFun(anon_fun) =>
                CmnExpr::AnonFun(conv_anon_fun(anon_fun)?),

            GhcExpr::FunCall(fun_call) =>
                CmnExpr::FunCall(conv_fun_call(fun_call)?),

            GhcExpr::LetExpr(let_expr) =>
                CmnExpr::LetExpr(conv_let_expr(let_expr)?),

            GhcExpr::PatMatch(pat_match) =>
                CmnExpr::PatMatch(conv_pat_match(pat_match)?),

            GhcExpr::LitConv(lit_conv) =>
                CmnExpr::LitVal(conv_lit_conv(lit_conv))
        };

    Ok(conv_expr)
}

/* AnonFun */
fn conv_anon_fun(anon_fun: &ghcir::AnonFun) -> Result<cmnir::AnonFun, ConvError> {
    fn conv_val_param(ghc_param: &ghcir::ValParam) -> cmnir::ValParam {
        cmnir::ValParam {
            name: conv_local(&ghc_param.name),
        }
    }

    let conv_val_params = anon_fun.val_params
                            .iter()
                            .map(conv_val_param)
                            .collect();
    let conv_body = conv_expr(&anon_fun.body)?;

    let conv_anon_fun =
        cmnir::AnonFun {
            val_params:       conv_val_params,
            body:             Box::new(conv_body),
        };

    Ok(conv_anon_fun)
}

/* FunCall */
fn conv_fun_call(ghc_fun_call: &ghcir::FunCall) -> Result<cmnir::FunCall, ConvError> {
    let conv_operator = resolve_operator(&ghc_fun_call)?;
    let conv_val_args = ghc_fun_call.val_args
                            .iter()
                            .map(conv_local)
                            .collect();

    let conv_fun_call =
        cmnir::FunCall {
            operator: conv_operator,
            val_args: conv_val_args,
        };

    Ok(conv_fun_call)
}

fn resolve_operator(ghc_fun_call: &ghcir::FunCall) -> Result<cmnir::Operator, ConvError> {
    /* TODO this might be better done as a separate lowering phase.
     * Right now, we don't support generic function calls so we can't
     * create the arbitrarily-named nodes.
     */
    /* 1. If the function call doesn't have arguments, this is just a local var copy. */
    if ghc_fun_call.val_args.len() == 0 {
        return Ok(Operator::Noop)
    }

    /* 2. Otherwise, check if we know this Haskell function */
    /* Note: user-defined function calls are currently not supported */
    let fun_name = &ghc_fun_call.called_fun;

    use cmnir::Operator;
    match &fun_name.0[0..] {
        "GHC.Num.fromInteger" =>
            Ok(Operator::Noop),  /* for now, we do nothing here */

        "GHC.Classes.<=" =>
            Ok(Operator::LessThanOrEqual),

        "GHC.Num.+" =>
            Ok(Operator::Plus),

        _ =>
            Err(ConvError::UnknownOperator(fun_name.clone())),
    }
}

/* LetExpr */
fn conv_let_expr(ghc_let: &ghcir::LetExpr) -> Result<cmnir::LetExpr, ConvError> {
    let conv_var_name  = conv_local(&ghc_let.var_name);
    let conv_var_value = conv_expr(&ghc_let.var_value)?;
    let conv_body      = conv_expr(&ghc_let.body)?;

    let conv_let =
        cmnir::LetExpr {
            var_name:  conv_var_name,
            var_value: Box::new(conv_var_value),
            body:      Box::new(conv_body),
        };

    Ok(conv_let)
}

/* PatMatch */
fn conv_pat_match(ghc_pat_match: &ghcir::PatMatch) -> Result<cmnir::PatMatch, ConvError> {
    let conv_matched_var = conv_local(&ghc_pat_match.matched_var);

    let num_cases = ghc_pat_match.pat_cases.len();
    if num_cases != 1 {
        return Err(ConvError::TooManyPatCases(num_cases));
    }

    let single_ghc_case = ghc_pat_match.pat_cases.get(0).unwrap();
    let conv_pat_case   = conv_pat_case(single_ghc_case)?;

    let conv_pat_match =
        cmnir::PatMatch {
            matched_var: conv_matched_var,
            pat_case:    conv_pat_case,
        };

    Ok(conv_pat_match)
}

/* PatCase */
fn conv_pat_case(ghc_pat_case: &ghcir::PatCase) -> Result<cmnir::PatCase, ConvError> {
    let conv_constructor = conv_global(&ghc_pat_case.constructor);
    let conv_field_binds = ghc_pat_case.field_binds
                            .iter()
                            .map(|mb_var| {
                                mb_var.as_ref()
                                    .map(conv_local)
                            })
                            .collect();
    let conv_body = conv_expr(&ghc_pat_case.body)?;

    let conv_pat_case =
        cmnir::PatCase {
            constructor: conv_constructor,
            field_binds: conv_field_binds,
            body:        Box::new(conv_body),
        };

    Ok(conv_pat_case)
}

/* LitConv */
fn conv_lit_conv(ghc_lit_conv: &ghcir::LitConv) -> cmnir::LitVal {
    match &ghc_lit_conv.raw_lit {
        ghcir::RawLit::IntLit(n) =>
            cmnir::LitVal::IntLit(*n)
    }
}

/* Global */
pub fn conv_global(ghc_global: &ghcir::Global) -> cmnir::Global {
    cmnir::Global(ghc_global.0.clone())
}

/* Local */
pub fn conv_local(ghc_local: &ghcir::Local) -> cmnir::Local {
    cmnir::Local(ghc_local.0.clone())
}
