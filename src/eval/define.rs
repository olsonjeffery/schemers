// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, List, Atom, Symbol};
use env::Env;
use result::SchemerResult;
use super::{lambda, eval};

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_define(cdr: Expr, env: Env) -> SchemerResult<Env> {
    match cdr {
        List(mut items) => {
            if items.len() != 2 {
                return Err("eval: define: should be two entries".to_string());
            }
            let item = *try_opt!(items.shift(),
                                "eval: define: var name should have value".to_string());
            match item {
                    Atom(Symbol(name)) => {
                        let val_expr =
                            *try_opt!(items.pop(),
                                     "eval: define: value should be there".to_string());
                        let (val_expr, mut env) = {
                            if val_expr.is_atom() {
                                try!(eval(val_expr , env))
                            } else {
                                let (car, cdr) = match val_expr.clone().un_cons() {
                                    Ok(v) => v,
                                    Err(e) => return Err(e)
                                };
                                match car {
                                    Atom(ref val) if *val ==
                                            Symbol("lambda".to_string()) => {
                                        let result = try!(
                                            lambda::eval_lambda(
                                                name.to_string(),
                                                cdr, env.clone()));
                                        (result, env)
                                    }
                                    _ => try!(eval(val_expr , env))
                                }
                            }
                        };
                        let val_expr = try_opt!(val_expr,
                            "eval: define: got None for val".to_string());
                        env.define(name, val_expr);
                        Ok(env)
                    },
                    _ => return Err("eval: define: atom in var pos. must be symbol".to_string())
                }
        },
        _ => return Err("eval: define: should be list in cdr position".to_string())
    }
}
