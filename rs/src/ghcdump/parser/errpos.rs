use std::cmp;

// ErrPos basics

#[derive(Debug, Clone)]
pub enum ErrPos {
    Point(*const u8),
    Range(*const u8, *const u8)
}

impl ErrPos {
    pub fn at(curr_input: &str) -> ErrPos {
        ErrPos::Point(curr_input.as_ptr())
    }

    pub fn between(prev_pos: ErrPos, curr_input: &str) -> Option<ErrPos> {
        use ErrPos::*;
        match prev_pos {
            Point(prev_ptr) =>
                Some(Range(prev_ptr, curr_input.as_ptr())),
            Range(..) =>
                None
        }
    }

    fn start_ptr(&self) -> *const u8 {
        use ErrPos::*;
        match self {
            Point(ptr)          => *ptr,
            Range(start_ptr, _) => *start_ptr
        }
    }
}

// Report building

pub fn report(err_pos: &ErrPos, full_input: &str) -> Result<String, Error> {
    use ErrPos::*;
    match err_pos {
        Point(ptr_ref) => {
            let ptr_pos = *ptr_ref;  /* we have a &*const u8 because err_pos is a ref */
            let input_pos = unsafe {
                index_in_str(ptr_pos, full_input)?
            };
            let curr_line = current_line(full_input, input_pos);
            let marker_line = build_marker_line(curr_line, ptr_pos)?;
            let report = format!("{}\n{}", curr_line, marker_line);
            Ok(report)
        }
        Range(_ptr_start, _ptr_end) =>
            Ok(String::new()) /* TODO */
    }
}

unsafe fn index_in_str(ptr: *const u8, slice: &str) -> Result<usize, Error> {
    let diff = ptr.offset_from(slice.as_ptr());

    if diff < 0 {
        return Err(Error::BeforeStrBegin);
    }

    let udiff = diff as usize;
    if udiff > slice.len() {
        /* slice.len() returns in bytes, we're all good */
        Err(Error::AfterStrEnd)
    }
    else {
        Ok(udiff)
    }
}

fn subslice(full_input: &str, begin_incl: usize, end_incl: usize) -> &str {
    /* Also include the character at the last mentioned position */
    let slice_end = cmp::min(end_incl+1, full_input.len());
    &full_input[begin_incl..slice_end]
}

fn current_line(full_input: &str, pos: usize) -> &str {
    let begin = current_line_begin(full_input, pos);
    let end = current_line_end(full_input, pos);
    subslice(full_input, begin, end)
}

fn current_line_begin(full_input: &str, pos: usize) -> usize {
    /* FIXME this doesn't work with \r */
    match full_input[..pos].rfind('\n') {
        Some(newline_pos) => newline_pos + 1,  /* skip the newline char */
        None              => 0,                /* no newline char,
                                                * so beginning of line is beginning
                                                * of the string
                                                */
    }
}

fn current_line_end(full_input: &str, pos: usize) -> usize {
    /* FIXME this doesn't work with \r */
    match full_input[pos..].find('\n') {
        /* FIXME something is wrong here:
         * Result size of CorePre
         * is understood as the current line
         * (missing the final 'p')
         */
        Some(newline_pos) => pos + newline_pos - 1,  /* skip the newline char */
        None              => full_input.len(),       /* no newline char,
                                                      * so end of line is end
                                                      * of the string
                                                      */
    }
}

fn build_marker_line(line: &str, ptr_pos: *const u8) -> Result<String, Error> {
    let str_pos = unsafe {
        index_in_str(ptr_pos, line)?
    };

    let prefix = " ".repeat(str_pos);
    let indicator = '^';
    /* thread '<unnamed>' panicked at 'attempt to subtract with overflow', src/ghcdump/parser/errpos.rs:106:29 */
    //let suffix = " ".repeat(line.len() - str_pos - 1);
    let suffix = match std::panic::catch_unwind(|| " ".repeat(line.len() - str_pos - 1)) {
        Ok(s) => s,
        Err(e) => {
            println!("Thread panicked!");
            println!("  line = \"{}\"", line);
            println!("  line.len() = {}", line.len());
            println!("  str_pos = {}", str_pos);
            String::from("")
        }
    };

    let marker_line = format!("{}{}{}", prefix, indicator, suffix);
    Ok(marker_line)
}

// Various external helpers

#[allow(unused_unsafe)]
pub unsafe fn is_at_beginning_of_line(err_pos: &ErrPos, full_input: &str) -> Result<bool, Error> {
    let pos_ptr = err_pos.start_ptr();

    let curr_pos = unsafe {
        index_in_str(pos_ptr, full_input)?
    };
    let line_begin = current_line_begin(full_input, curr_pos);

    Ok(line_begin == curr_pos)
}

#[allow(unused_unsafe)]
pub unsafe fn retrieve_slice<'a, 'b>(err_pos: &'a ErrPos, full_input: &'b str)
    -> Result<&'b str, Error>
{
    use ErrPos::*;
    let (begin, end) = match err_pos {
        Point(ptr) => {
            let begin = unsafe {
                index_in_str(*ptr, full_input)?
            };
            let end = full_input.len();
            (begin, end)
        }

        Range(begin_ptr, end_ptr) => {
            unsafe {
                let begin = index_in_str(*begin_ptr, full_input)?;
                let end = index_in_str(*end_ptr, full_input)?;
                (begin, end)
            }
        }
    };

    let subslice = subslice(full_input, begin, end);
    Ok(subslice)
}

// Line number

pub unsafe fn line_number(err_pos: &ErrPos, input: &str) -> Option<usize> {
    let mut n = 1;
    let err_ptr = err_pos.start_ptr();

    if index_in_str(err_ptr, input).is_err() {
        /* The start pointer is not even in the full input */
        return None;
    }

    for ln in input.lines() {
        match index_in_str(err_ptr, ln) {
            Ok(_) =>                      /* found it, return */
                return Some(n),

            Err(Error::AfterStrEnd) =>    /* further, keep iterating */
                n += 1,

            Err(Error::BeforeStrBegin) => /* impossible, raise an error */
                unreachable!("line_number: unreachable state 1"),
        }
    }

    /* We can't reach here: the pointer was validated to be
     * in the full string.
     */
    unreachable!("line_number: unreachable state 2");
}

// Error composition

/* This is a pretty bad comparison behaviour */
/* The better approach seems to be to return Option<Ordering>,
 * where a point ending inside a range doesn't compare.
 */
pub unsafe fn compare(err_pos1: &ErrPos, err_pos2: &ErrPos) -> std::cmp::Ordering {
    /* Should we use the end position here? */
    let ptr1 = err_pos1.start_ptr();
    let ptr2 = err_pos2.start_ptr();
    let diff = ptr1.offset_from(ptr2);

    isize::cmp(&diff, &0)
}

// Error

pub enum Error {
    BeforeStrBegin,
    AfterStrEnd
}
