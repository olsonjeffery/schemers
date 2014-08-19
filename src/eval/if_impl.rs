// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use super::{EvalResult, eval};
use expr::{Expr, Atom, Boolean, List};
use env::Env;

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_if(cdr: Expr, env: Env) -> EvalResult {
    match cdr {
        List(mut items) => {
            if items.len() != 3 {
                return Err("eval: if: should be three entries in if cdr list".to_string());
            }
            let first_arg = *try_opt!(items.remove(0),
                "eval: if: should have some val in arg first position".to_string());
            let (out_expr, out_env) = try!(eval(
                first_arg, env));
            let out_branch = match out_expr {
                // alt branch -- only returned if test returns false
                Some(Atom(Boolean(false))) => items.pop(),
                // conseq branch -- returned on all other results
                _ => items.remove(0),
            };
            return Ok((out_branch.map(|n| *n), out_env))
        },
        _ => return Err("eval: if: should have List in cdr position".to_string())
    }
}
