// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, List, Atom, Symbol, Lambda, UserDefined};
use env::Env;
use result::SchemerResult;

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_lambda(name: String, cdr: Expr, env: Env) -> SchemerResult<Option<Expr>> {
    match cdr {
        List(mut items) => {
            if items.len() != 2 {
                return Err("eval: lambda: should be two entries in cdr".to_string());
            }
            let mut var_names = Vec::new();
            let item = *try_opt!(items.shift(),
                                "eval: lambda: vars should be there".to_string());
            match item {
                List(vars) => {
                    for v in vars.move_iter() {
                        match v {
                            box Atom(Symbol(var_name)) => {
                                var_names.push(var_name.to_string());
                            },
                            _ => return Err("eval: lambda: var names must be symbols".to_string())
                        }
                    }
                }, _ => return Err("eval: lambda: should have vars to be list".to_string())
            }
            let item = try_opt!(items.shift(),
                                "eval: lambda: lambda body should be there".to_string());
            Ok(Some(Atom(Lambda(UserDefined(name, var_names, item, env)))))
        },
        _ => return Err("eval: lambda: should be list in cdr position".to_string())
    }
}
