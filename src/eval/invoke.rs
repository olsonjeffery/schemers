// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use env::Env;
use expr::{Expr, Lambda, UserDefined, BuiltIn, List, Atom};
use super::{eval, EvalResult};

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_invoke(proc_name: String, cdr: Expr, env: Env) -> EvalResult {
    let target_proc = match env.find(&proc_name) {
        Ok(expr) => expr,
        Err(err) =>
            return Err(format!("eval: error w/ resolve symbol: {}", err))
    };
    match target_proc {
        Atom(Lambda(UserDefined(_, vars, body, captured_env))) => {
            match cdr {
                args @ List(_) => {
                    let (args, _) = try!(args.unbox_and_eval(
                            env.clone()));
                    let mut active_env = captured_env.clone();
                    active_env.enclose(env.clone());
                    let active_env = try!(Env::new(Some(vars),
                                                   Some(args),
                                                   Some(active_env)));
                    let (out_expr, _) = try!(eval(*body, active_env));
                    Ok((out_expr, env))
                },
                _ => return Err("eval: proc invoke: should've \
                                gotten a list in the cdr".to_string())
            }
        },
        Atom(Lambda(BuiltIn(_, _body_fn))) => {
            match cdr {
                list @ List(_) => {
                    let (args, env) = match list.unbox_and_eval(env) {
                        Ok(r) => r,
                        Err(e) => return Err(
                            format!("eval(): error in unbox: {}", e))
                    };
                    let result = try!(_body_fn(args, env));
                    Ok(result)
                },
                _ => return Err("cdr should be List(...)".to_string())
            }
        }, _ => return Err(format!("defined var {} is not a procedure!", proc_name))
    }
}
