/* original code */
fn fib2(n: u32) -> u32 {
  let mut prev = 1;
  let mut prevPrev = 0;
  let mut curr = prev;

  for i in 1..n {
    curr = prev + prevPrev;
    prevPrev = prev;
    prev = curr;
  }

  curr
}

/* lowering 1: for lowering */
fn fib2(n: u32) -> u32 {
  let mut prev = 1;
  let mut prevPrev = 0;
  let mut curr = prev;

  let mut i = 1;
  while i < n {
    curr = prev + prevPrev;
    prevPrev = prev;
    prev = curr;
    i += 1;
  }

  curr
}

/* lowering 2: explicit readMut */
fn fib2(n: u32) -> u32 {
  let mut prev = 1;
  let mut prevPrev = 0;
  let mut curr = readMut$ prev;

  let mut i = 1;
  while (readMut$ i) < n {
    curr = (readMut$ prev) + (readMut$ prevPrev);
    prevPrev = (readMut$ prev);
    prev = (readMut$ curr);
    i = (readMut$ i) + 1;
  }

  (readMut$ curr)
}

/* lowering 3: sub expressions into let */
fn fib2(n: u32) -> u32 {
  let mut prev = 1;
  let mut prevPrev = 0;
  let mut curr = readMut$ prev;

  let mut i = 1;
  while (
    let while_1 = readMut$ i;
    let while_2 = while_1 < n;
    while_2
  )
  {
    let curr_1 = readMut$ prev;
    let curr_2 = readMut$ prevPrev;
    let curr_3 = curr_1 + curr_2;
    curr = curr_3;

    let prevPrev_1 = readMut$ prev;
    prevPrev = prevPrev_1;

    let prev_1 = readMut$ curr;
    prev = prev_1;

    let i_1 = readMut$ i;
    let i_2 = 1;
    let i_3 = i_1 + i_2;
    i = i_3;
  }

  let ret_1 = readMut$ curr;
  ret_1
}
