use super::{RuntimeError, ColId, Rowid, DataGuide, CurKind, Cursor, CurFilter, CurRead};
use std::collections::HashMap;
use crate::ir;
use crate::fql::{self, QPlan, QReadT, QFilter};
use super::qeval;

/* RowFormat / RowVal */

enum RowFormat {
    RowScalar(RowScalar),
    RowComposite(Vec<RowFormat>)
}

enum RowScalar {
    RowFromTable(ColId, RtColIdx),  /* RtCol contains RowId */
    RowInline(RtColIdx)             /* RtCol contains RtVal */
}

struct RowVal(Vec<RtCol>);

type RtCol = Vec<u8>; /* TODO change to Box<[u8]> */

struct RtBag { rt_cols: Vec<RtCol> }

type RtColIdx = usize;

fn alloc_rt_cols(row_fmt: &RowFormat) -> RtBag {
    fn register_rtcol<T: Sized>(rtcol_map: &mut HashMap<RtColIdx, usize>, rt_idx: RtColIdx) {
        let rtcol_size = std::mem::size_of::<T>();
        let conflict = rtcol_map.insert(rt_idx, rtcol_size);
        assert!(conflict.is_none());
    }

    /* Recursively go through the RowFormat and compute the size of each RtCol */
    fn discover_rtcols(row_fmt: &RowFormat, rtcol_map: &mut HashMap<RtColIdx, usize>) {
        match row_fmt {
            RowFormat::RowScalar(
                RowScalar::RowFromTable(_, rt_idx)) =>
            {
                register_rtcol::<Rowid>(rtcol_map, *rt_idx);
            }

            RowFormat::RowScalar(
                RowScalar::RowInline(rt_idx)) =>
            {
                register_rtcol::<qeval::RtVal>(rtcol_map, *rt_idx);
            }

            RowFormat::RowComposite(fields) => {
                for field_format in fields {
                    discover_rtcols(field_format, rtcol_map);
                }
            }
        }
    }

    let mut rtcol_map = HashMap::<RtColIdx, usize>::new();
    let nentries = *rtcol_map.keys()
                        .max()
                        .unwrap();
    let mut rt_cols = Vec::new();

    /* Doing in order ensures that:
     * (1) we can create the buffer via push instead of allocating zeroed memory
     * (2) we ensure that there is no gap in the index map
     */
    for rt_idx in 0..nentries {
        let rtcol_size = *rtcol_map.get(&rt_idx).unwrap();
        let rtcol = vec![0; rtcol_size]; /* 0-initialize the memory */
        rt_cols.push(rtcol);
    }

    RtBag { rt_cols }
}

type FieldIdx = usize;

fn rtcol_idx_from_scalar_row(scalar_fmt: &RowScalar) -> RtColIdx {
    match scalar_fmt {
        RowScalar::RowFromTable(_, rt_idx) => *rt_idx,
        RowScalar::RowInline(rt_idx)       => *rt_idx,
    }
}

fn find_rtcol_idx(row_fmt: &RowFormat, field_path: &[FieldIdx]) -> Result<RtColIdx, RuntimeError> {
    let mut curr_fmt = row_fmt;
    for field_idx in field_path {
        match curr_fmt {
            RowFormat::RowScalar(..) => {
                return Err(RuntimeError::ScalarRowFormatHasNoFields);
            }

            RowFormat::RowComposite(row_fields) => {
                /* resolve the access path at this level */
                curr_fmt = row_fields.get(*field_idx).unwrap();
            }
        }
    }

    /* Done with the access path. The row format at this level must
     * be a scalar.
     */
    match curr_fmt {
        RowFormat::RowScalar(scalar_fmt) => {
            Ok(rtcol_idx_from_scalar_row(scalar_fmt))
        }

        RowFormat::RowComposite(..) => {
            Err(RuntimeError::FieldPathIncompletelyResolved)
        }
    }
}

fn rtbag_write<T>(bag: &mut RtBag, row_fmt: &RowFormat, field_path: &[FieldIdx], value: T)
    -> Result<(), RuntimeError>
{
    let rtcol_idx = find_rtcol_idx(row_fmt, field_path)?;
    let rtcol = bag.rt_cols.get_mut(rtcol_idx).unwrap();
    /* To write, just see the buffer as a pointer to T, then write to it */
    let buf_ptr = rtcol.as_mut_ptr() as *mut T;
    unsafe {
        *buf_ptr = value;
    }
    Ok(())
}

fn rtbag_read<'a, T>(bag: &'a RtBag, row_fmt: &RowFormat, field_path: &[FieldIdx])
    -> Result<&'a T, RuntimeError>
{
    let rtcol_idx = find_rtcol_idx(row_fmt, field_path)?;
    let rtcol = bag.rt_cols.get(rtcol_idx).unwrap();
    /* To read, just see the buffer as a pointer to T, then read the value */
    let buf_ptr = rtcol.as_ptr() as *const T;
    unsafe {
        Ok(&*buf_ptr)
    }
}


/* Top-level entry point */

fn apply_data_accesses_cur_read(cur_read: &CurRead) -> Result<DataGuide, RuntimeError> {
    super::new_data_guide(&cur_read.tab_name)
}

/* TODO add support for it in the interpreter runtime */
const DATA_ACCESS_FUN_NAME: &str = "data_access$";

fn data_access_expr(dg_entry: usize, data_guide: &DataGuide) -> ir::Expr {
    let col_locator = data_guide.0.get(dg_entry).unwrap();

    let op = ir::Operator::ReadRtCol(col_locator.clone());

    let fun_call = ir::FunCall {
        operator: op,
        val_args: Vec::new(),
    };

    ir::Expr::FunCall(fun_call)
}

fn rec_apply_data_accesses_expr_box(
    expr:             Box<ir::Expr>,
    data_access_vars: &mut HashMap<ir::Local, &DataGuide>)
    -> Box<ir::Expr>
{
    let new_expr = rec_apply_data_accesses_expr(*expr, data_access_vars);

    Box::new(new_expr)
}

/* TODO: change to &mut ir::Expr and use '*expr = ...' */
fn rec_apply_data_accesses_expr(expr: ir::Expr, data_access_vars: &mut HashMap<ir::Local, &DataGuide>)
    -> ir::Expr
{
    /* Code mostly copied from crate::fql::interpreter::rec_interpret_row_expr */
    use ir::Expr::*;
    match expr {
        PatMatch(mut pat_match) => {
            /* Are we matching against a row var? */
            match data_access_vars.get(&pat_match.matched_var) {
                Some(data_guide) => {
                    /* Replace field references by data accesses
                     *
                     *   match s {
                     *     MyStruct(a, b) => f(a, b)
                     *   }
                     *
                     * ===>
                     *
                     *   let a = DataAccess(..)
                     *   let b = DataAccess(..)
                     *   f(a, b)
                     */
                    /* Start by recursing in the body */
                    /* FIXME with DataGuide, we only support one-level structs, not nested */
                    let matched_var = pat_match.matched_var;
                    let mut pat_case = pat_match.pat_case;
                    let new_body = rec_apply_data_accesses_expr(*pat_case.body, data_access_vars);

                    /* TODO would be better to loop in reverse order here */
                    let field_binds = pat_case.field_binds;
                    let mut replaced_expr = new_body;
                    let mut field_index = 0;
                    for field_bind in field_binds.into_iter() {
                        if field_bind.is_none() {
                            /* this index is not bound */
                            field_index += 1;
                            continue;
                        }
                        let bind_name = field_bind.unwrap();

                        let data_guide = data_access_vars.get(&matched_var).unwrap();
                        let data_access = data_access_expr(field_index, data_guide);

                        replaced_expr =
                            LetExpr (
                                ir::LetExpr {
                                    var_name:  bind_name,
                                    var_value: Box::new(data_access),
                                    body:      Box::new(replaced_expr),
                                }
                            );
                        field_index += 1;
                    }

                    replaced_expr
                }

                None => {
                    /* Not a row var, nothing to do */
                    /* Just recurse in the body */
                    let mut pat_case = pat_match.pat_case;
                    pat_case.body = rec_apply_data_accesses_expr_box(pat_case.body, data_access_vars);
                    pat_match.pat_case = pat_case;

                    PatMatch(pat_match)
                }
            }
        }

        AnonFun(mut anon_fun) => { /* can we use 'ref mut' here? */
            /* Just recurse in the body */
            anon_fun.body = rec_apply_data_accesses_expr_box(anon_fun.body, data_access_vars);
            AnonFun(anon_fun)
        }

        FunCall(ref fun_call) => {
            /* Any argument might be in the access vars.
             * If so, we would need to read all the columns and form
             * the runtime flat struct representation, and pass that
             * as argument to the function call. Note that we're normally
             * past the time for making inlining decisions.
             *
             * Currently, we don't support operators or function calls
             * taking struct as argument, so we just validate that
             * this case doesn't happen.
             */
            assert!(!fun_call.val_args
                        .iter()
                        .any(|v| data_access_vars.contains_key(v)));
                        /* should return Err here, annoying for now */
            /*if fun_call.val_args
                .iter()
                .any(|v| data_access_vars.contains_key(v))
            {
                return Err(RuntimeError::FlatStructFormatNotAvailable)
            }*/

            expr
        }

        LetExpr(mut let_expr) => {
            /* We don't support "copy assignment", e.g.
             *
             *   let a = b;
             *
             * Only way would be to use a Noop operator call, but
             * we don't support it for structs currently.
             *
             * For 'let', only need to recurse in both def expr and
             * body.
             */
            let_expr.var_value = rec_apply_data_accesses_expr_box(let_expr.var_value, data_access_vars);
            let_expr.body      = rec_apply_data_accesses_expr_box(let_expr.body,      data_access_vars);

            LetExpr(let_expr)
        }

        LitVal(..) => {
            expr  /* never modified */
        }
    }
}

fn apply_data_accesses_fun_code(orig_code: ir::AnonFun, input_dg: &DataGuide)
    -> Result<ir::Expr, RuntimeError>
{
    /* need some context here, no? */
    /* TODO init the context / code with the function arguments */
    /* code copied from crate::fql::interpreter::interpret_row_fun */
    let mut val_params = orig_code.val_params;
    if val_params.len() != 1 {
        return Err(RuntimeError::TooManyArguments(val_params.len()));
    }

    let param = val_params.remove(0); /* own the vec entry */
    let param_name = param.name;

    let fun_body = orig_code.body;

    if input_dg.0.len() == 1 {
        /* Input is a scalar.
         * Easy case: just add a 'let' expression with the data access.
         *
         *   f x = x + 1
         *
         * ===>
         *
         *   let x = DataAccess(..)
         *   x + 1
         */

        let data_access = data_access_expr(0, input_dg);

        let new_code =
            ir::Expr::LetExpr (
                ir::LetExpr {
                    var_name:  param_name,
                    var_value: Box::new(data_access),
                    body:      fun_body,
                }
            );

        Ok(new_code)
    }
    else {
        let mut data_access_vars = HashMap::new();

        let conflict = data_access_vars.insert(param_name, input_dg);
        assert!(conflict.is_none());

        let new_code = rec_apply_data_accesses_expr(*fun_body, &mut data_access_vars);
        Ok(new_code)
    }
}

fn apply_data_accesses_cur_filter(filter_node: &mut CurFilter)
    -> Result<DataGuide, RuntimeError>
{
    /* Step 1: Get row format from child node */
    let child_dg = apply_data_accesses(&mut filter_node.child_cursor)?;

    /* Step 2: Replace reads in filter code */
    /* TODO turn this into QFilter methods */
    let orig_filter_code = &filter_node.filter_code;
    let mut new_filter_code = match orig_filter_code {
        ir::Expr::AnonFun(anon_fun) => anon_fun.clone(), /* TODO remove this clone */
        /* At this point, this must be an anon fun node */
        _  => return Err(RuntimeError::NotAFunction)
    };
    let new_filter_code = apply_data_accesses_fun_code(new_filter_code, &child_dg)?;
    filter_node.filter_code = new_filter_code;

    Ok(child_dg)  /* filter still reads from the same location as child (no map) */
}

/* TODO rename
 *   explicit_data_access?
 */
pub fn apply_data_accesses(cursor: &mut Cursor) -> Result<DataGuide, RuntimeError> {
    match &mut cursor.cur_kind {
        CurKind::Read(cur_read) =>
            apply_data_accesses_cur_read(&cur_read),

        CurKind::Filter(cur_filter) =>
            apply_data_accesses_cur_filter(cur_filter),
    }
}
