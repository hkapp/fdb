use crate::ghcdump::ir as ghcir;
use crate::ir as cmnir;

pub type ConvError = String;

/* Decl */
pub fn conv_decl(ghc_decl: &ghcir::Decl) -> Result<cmnir::Decl, ConvError> {
    let conv_decl = cmnir::Decl {
        name: conv_global(&ghc_decl.name),
        body: conv_expr(&ghc_decl.body),
    };
    Ok(conv_decl)
}

/* Expr */
fn conv_expr(ghc_expr: &ghcir::Expr) -> cmnir::Expr {
    use ghcir::Expr as GhcExpr;
    use cmnir::Expr as CmnExpr;
    match ghc_expr {
        GhcExpr::AnonFun(anon_fun) =>
            CmnExpr::AnonFun(conv_anon_fun(anon_fun)),

        GhcExpr::FunCall(fun_call) =>
            CmnExpr::FunCall(conv_fun_call(fun_call)),

        GhcExpr::LetExpr(let_expr) =>
            CmnExpr::LetExpr(conv_let_expr(let_expr)),

        GhcExpr::PatMatch(pat_match) =>
            CmnExpr::PatMatch(conv_pat_match(pat_match)),

        GhcExpr::LitConv(lit_conv) =>
            CmnExpr::LitConv(conv_lit_conv(lit_conv))
    }
}

/* AnonFun */
fn conv_anon_fun(anon_fun: &ghcir::AnonFun) -> cmnir::AnonFun {
    fn conv_val_param(ghc_param: &ghcir::ValParam) -> cmnir::ValParam {
        cmnir::ValParam {
            name: conv_local(&ghc_param.name),
            typ:  ()
        }
    }

    let conv_val_params = anon_fun.val_params
                            .iter()
                            .map(conv_val_param)
                            .collect();
    let conv_body = conv_expr(&anon_fun.body);

    cmnir::AnonFun {
        type_params:      (),
        typeclass_params: (),
        val_params:       conv_val_params,
        body:             Box::new(conv_body),
    }
}

/* FunCall */
fn conv_fun_call(ghc_fun_call: &ghcir::FunCall) -> cmnir::FunCall {
    let conv_called_fun     = conv_global(&ghc_fun_call.called_fun);
    let conv_type_args      = Vec::new();
    let conv_typeclass_args = Vec::new();

    let conv_val_args = ghc_fun_call.val_args
                            .iter()
                            .map(conv_local)
                            .collect();

    cmnir::FunCall {
        called_fun:     conv_called_fun,
        type_args:      conv_type_args,
        typeclass_args: conv_typeclass_args,
        val_args:       conv_val_args,
    }
}

/* LetExpr */
fn conv_let_expr(ghc_let: &ghcir::LetExpr) -> cmnir::LetExpr {
    let conv_var_name  = conv_local(&ghc_let.var_name);
    let conv_var_value = conv_expr(&ghc_let.var_value);
    let conv_body      = conv_expr(&ghc_let.body);

    cmnir::LetExpr {
        var_name:  conv_var_name,
        var_type:  (),
        var_value: Box::new(conv_var_value),
        body:      Box::new(conv_body),
    }
}

/* PatMatch */
fn conv_pat_match(ghc_pat_match: &ghcir::PatMatch) -> cmnir::PatMatch {
    let conv_matched_var = conv_local(&ghc_pat_match.matched_var);
    let conv_pat_cases   = ghc_pat_match.pat_cases
                            .iter()
                            .map(conv_pat_case)
                            .collect();

    cmnir::PatMatch {
        matched_var: conv_matched_var,
        pat_cases:   conv_pat_cases,
    }
}

/* PatCase */
fn conv_pat_case(ghc_pat_case: &ghcir::PatCase) -> cmnir::PatCase {
    let conv_constructor = conv_global(&ghc_pat_case.constructor);
    let conv_field_binds = ghc_pat_case.field_binds
                            .iter()
                            .map(|mb_var| {
                                mb_var.as_ref()
                                    .map(conv_local)
                            })
                            .collect();
    let conv_body = conv_expr(&ghc_pat_case.body);

    cmnir::PatCase {
        constructor: conv_constructor,
        field_binds: conv_field_binds,
        body:        Box::new(conv_body),
    }
}

/* LitConv */
fn conv_lit_conv(ghc_lit_conv: &ghcir::LitConv) -> cmnir::LitConv {
    fn conv_raw_lit(ghc_lit: &ghcir::RawLit) -> cmnir::RawLit {
        match ghc_lit {
            ghcir::RawLit::IntLit(n) =>
                cmnir::RawLit::IntLit(*n)
        }
    }

    cmnir::LitConv {
        conv_fun: conv_global(&ghc_lit_conv.conv_fun),
        raw_lit:  conv_raw_lit(&ghc_lit_conv.raw_lit),
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
