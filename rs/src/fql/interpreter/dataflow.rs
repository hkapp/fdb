use super::{QPlan, RuntimeError, ColId, Rowid, RtVal, DataGuide};
use std::collections::HashMap;
use crate::ghcdump::ir;
use crate::objstore;
use crate::fql;

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
                register_rtcol::<RtVal>(rtcol_map, *rt_idx);
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

fn apply_data_accesses_qread(tab_name: &str) -> DataGuide {
    super::new_data_guide(tab_name)
}

/* TODO add support for it in the interpreter runtime */
const DATA_ACCESS_FUN_NAME: &str = "data_access$";

fn data_access_expr(dg_entry: usize, data_guide: &DataGuide) -> Box<ir::Expr> {
    let col_locator = data_guide.0.get(dg_entry).unwrap();

    let tab_name_expr = ir::Expr::RawLit(
        ir::RawLit::StrLit(
            String::from(col_locator.tab_name))
    );

    let col_name_expr = ir::Expr::RawLit(
        ir::RawLit::StrLit(
            String::from(col_locator.col_name))
    );

    let fun_call = ir::FunCall {
        called_fun:     ir::Global::from(String::from(DATA_ACCESS_FUN_NAME)),
        type_args:      Vec::new(), /* we would actually want some type arguments here */
        typeclass_args: Vec::new(),
        val_args:       vec![tab_name_expr, col_name_expr] /* FIXME add support for inline values */
    };

    Box::from(fun_call)
}

fn rec_apply_data_accesses_expr(expr: &mut ir::Expr, data_access_vars: HashMap<ir::Local, DataGuide>) {
    /* Code mostly copied from crate::fql::interpreter::rec_interpret_row_expr */
    use ir::Expr::*;
    match expr {
        PatMatch( ir::PatMatch { matched_var, pat_cases } ) => {
            if pat_cases.len() > 1 {
                /* FIXME we currently don't support product types, only pure sum types */
                /* FIXME should be a compile error */
                return Err(RuntimeError::TooManyCases(pat_cases.len()));
            }

            let deconstruct = pat_cases.get(0).unwrap();
            let field_binds = &deconstruct.field_binds;

            match data_access_vars.get(matched_var) {
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
                    /* FIXME with DataGuide, we only support one-level structs, not nested */
                    /* TODO would be better to loop in reverse order here */
                    let mut replaced_expr = &mut deconstruct.body;
                    for field_index in 0..field_binds.len() {
                        let field_bind = field_binds.get(field_index).unwrap();

                        if field_bind.is_none() {
                            /* this index is not bound */
                            continue;
                        }
                        let field_bind = field_bind.as_ref().unwrap();

                        let data_access = data_access_expr(field_index, data_guide);
                        replaced_expr =
                            LetExpr (
                                ir::LetExpr {
                                    var_name:  field_bind.name,
                                    var_type:  field_bind.typ,
                                    var_value: data_access_expr,
                                    body:      replaced_expr,
                                }
                            );
                    }

                    replaced_expr  /* use that to continue recursing */
                }
                None => { /* nothing to do */ }
            }
            /* Recurse in the body */
            rec_apply_data_accesses_expr(&mut deconstruct.body, data_access_vars);
        }
    }
}

fn apply_data_accesses_fun_code(orig_code: &ir::AnonFun, input_dg: &DataGuide)
    -> Result<ir::Expr, RuntimeError>
{
    /* need some context here, no? */
    /* TODO init the context / code with the function arguments */
    /* code copied from crate::fql::interpreter::interpret_row_fun */
    let val_params = &orig_code.val_params;
    if val_params.len() != 1 {
        return Err(RuntimeError::TooManyArguments(val_params.len()));
    }

    let param = val_params.get(0).unwrap();
    let param_name: &ir::Local = &param.name;

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

        ir::LetExpr {
            var_name:  param_name,
            var_type:  param.typ,
            var_value: data_access,
            body:      Box::from(orig_code.body.clone()),
        }
    }
    else {
        let mut data_access_vars = HashMap::new();
        let mut new_code = orig_code.body.clone();

        let conflict = data_access_vars.insert(param_name, input_dg);
        assert!(conflict.is_none());

        rec_apply_data_accesses_expr(&mut new_code, &mut data_access_vars)
    }
}

fn apply_data_accesses_qfilter(filter_fun: &objstore::Obj, qchild: &QPlan)
    -> Result<DataGuide, RuntimeError>
{
    /* Step 1: Get row format from child node */
    let child_dg = apply_data_accesses(qchild);

    /* Step 2: Replace reads in filter code */
    let filter_decl = fql::extract_decl(filter_fun)?;
    let orig_filter_code = fql::check_is_fun_decl(filter_decl)?;
    let new_filter_code = apply_data_accesses_fun_code(orig_filter_code, child_dg);
}

pub fn apply_data_accesses(qplan: &mut QPlan) -> RowFormat {
    match qplan {
        QPlan::Read { tab_name } =>
            apply_data_accesses_qread(&tab_name),

        QPlan::Filter { filter_fun, qchild } =>
            apply_data_accesses_qfilter(&filter_fun, &qchild),
    }
}
