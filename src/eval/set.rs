// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, List, Atom, Symbol};
use env::Env;
use super::{eval, EvalResult};

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_set(cdr: Expr, env: Env) -> EvalResult {
    match cdr {
        List(mut items) => {
            if items.len() != 2 {
                return Err("eval: set!: should have two entries".to_string());
            }
            let set_val = *try_opt!(items.shift(),
                                    "eval: set!: var name should have value".to_string());
            match set_val {
                Atom(Symbol(name)) => {
                    let item = *try_opt!(items.pop(),
                                         "eval: set! val not provided".to_string());
                    let (val_expr, mut env) =
                        try!(eval(item, env));

                    let val_expr = try_opt!(val_expr,
                                            "eval: set!: provided val didn't resolve".to_string());
                    try!(env.set(name, val_expr));
                    Ok((None, env))
                },
                _ => return Err("eval: set!: atom in var name \
                                position must be symbol".to_string())
            }
        },
        _ => return Err("eval: set!: should have list in cdr position".to_string())
    }
}
