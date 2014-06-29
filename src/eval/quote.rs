// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
use expr::{Expr, List};
use env::Env;
use super::{EvalResult};

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_quote(cdr: Expr, env: Env) -> EvalResult {
    match cdr {
        List(mut items) => {
            let quoted_item =
                *try_opt!(items.shift(),
                          "eval: quote list shouldnt be empty".to_string());
            Ok((Some(quoted_item), env))
        },
        _ => return Err("eval: quote: should have List \
                        in cdr position.".to_string())
    }
}
