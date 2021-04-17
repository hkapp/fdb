use std::ptr;

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

    /* This is unsafe because we're using the NULL pointer.
     * The resulting ErrPos should never be used to call "report".
     */
    pub unsafe fn none() -> ErrPos {
        ErrPos::Point(ptr::null())
    }
}

// Report building

pub fn report(err_pos: ErrPos, full_input: &str) -> Result<String, Error> {
    use ErrPos::*;
    match err_pos {
        Point(ptr_pos) => {
            let input_pos = unsafe {
                index_in_str(ptr_pos, full_input)?
            };
            let curr_line = current_line(full_input, input_pos);
            let marker_line = build_marker_line(curr_line, ptr_pos)?;
            let report = format!("{}\n{}", curr_line, marker_line);
            Ok(report)
        }
        Range(ptr_start, ptr_end) =>
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

fn current_line(full_input: &str, pos: usize) -> &str {
    let begin = current_line_begin(full_input, pos);
    let end = current_line_end(full_input, pos);
    &full_input[begin..end]
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
    let suffix = " ".repeat(line.len() - str_pos - 1);

    let marker_line = format!("{}{}{}", prefix, indicator, suffix);
    Ok(marker_line)
}

// Error

pub enum Error {
    BeforeStrBegin,
    AfterStrEnd
}
