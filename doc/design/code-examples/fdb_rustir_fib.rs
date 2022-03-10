/* original code */
fn fib(n: u32) -> u32 {
  if n <= 1 {
    return n;
  }
  else {
    return fib(n-1) + fib(n-2);
  }
}

/* modified code: no support for return */
fn fib(n: u32) -> u32 {
  if n <= 1 {
    n
  }
  else {
    fib(n-1) + fib(n-2)
  }
}

/* lowering 1: sub expressions into let */
fn fib(n: u32) -> u32 {
  let if_1 = 1;
  let if_2: bool = (n <= if_1);
  if if_2 {
    n
  }
  else {
    let ret_1 = 1;
    let ret_2 = n - ret_1;
    let ret_3 = fib(ret_2);
    let ret_4 = 2;
    let ret_5 = n - ret_4;
    let ret_6 = fib(ret_5);
    let ret_7 = ret_3 + ret_6;
    ret_7
  }
}

/* lowering 2: if -> pattern match */
fn fib(n: u32) -> u32 {
  let if_1 = 1;
  let if_2: bool = (n <= if_1);
  match if_2 {
    True => {
      n
    }
    False => {
      let ret_1 = 1;
      let ret_2 = n - ret_1;
      let ret_3 = fib(ret_2);
      let ret_4 = 2;
      let ret_5 = n - ret_4;
      let ret_6 = fib(ret_5);
      let ret_7 = ret_3 + ret_6;
      ret_7
    }
  }
}
