use super::{QPlan, RuntimeError, ColId, Rowid, RtVal};
use std::collections::HashMap;

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

//fn apply_data_accesses_qread(tab_name: &str) -> RowFormat {
    //TODO
//}

//fn apply_data_accesses_qfilter(fun_name: &str, )

//pub fn apply_data_accesses(qplan: &mut QPlan) -> RowFormat {
    /* Step 1: get row format from child node */
    //match qplan {
        //QPlan::Read(tab_name) => ..;
        //QPlan::Filter(fun_name, child_query) => ..;
    //}
//}
