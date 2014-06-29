// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{EvalResult, eval};
use expr::{Expr, List};
use env::Env;

pub fn eval_begin(cdr: Expr, env: Env) -> EvalResult {
    match cdr {
        List(items) => {
            let (mut out_expr, mut env) = (None, env);
            for e in items.move_iter() {
                let (res_expr, res_env) = try!(eval(*e, env));
                out_expr = res_expr;
                env = res_env;
            }
            Ok((out_expr, env))
        },
        _ => return Err("eval: begin: expcted a list for the input cdr".to_string())
    }
}
