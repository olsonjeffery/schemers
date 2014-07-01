// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{EvalResult, eval};
use expr::{Expr, List};
use env::Env;

pub fn eval_begin(cdr: Expr, mut env: Env) -> EvalResult {
    match cdr {
        List(items) => {
            let mut ctr = 1;
            let total_len = items.len();
            for e in items.move_iter() {
                if ctr == total_len {
                    return Ok((Some(*e), env))
                } else {
                    let (_, res_env) = try!(eval(*e, env));
                    env = res_env;
                    ctr += 1;
                }
            }
            return Err("eval: begin: Exited loop, shouldn't happen".to_string())
        },
        _ => return Err("eval: begin: expcted a list for the input cdr".to_string())
    }
}
