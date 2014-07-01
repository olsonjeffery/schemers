// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use env::Env;
use expr::{Expr, Lambda, UserDefined, BuiltIn, List, Atom};
use super::EvalResult;

pub enum InvokeResult {
    UserDefinedInvoke(EvalResult),
    BuiltInInvoke(EvalResult)
}

pub fn eval_invoke(proc_name: String, cdr: Expr, env: Env) -> InvokeResult {
    let target_proc = match env.find(&proc_name) {
        Ok(expr) => expr,
        Err(err) =>
            return UserDefinedInvoke(Err(format!("eval: error w/ resolve symbol: {}", err)))
    };
    match target_proc {
        Atom(Lambda(UserDefined(_, vars, body, captured_env))) => {
            match cdr {
                args @ List(_) => {
                    let (args, _) = match args.unbox_and_eval(
                            env.clone()) {
                        Ok(a) => a,
                        Err(e) => return UserDefinedInvoke(Err(e))
                    };
                    let mut active_env = captured_env.clone();
                    active_env.enclose(env.clone());
                    let active_env = match Env::new(Some(vars),
                                                   Some(args),
                                                   Some(active_env)) {
                        Ok(e) => e,
                        Err(e) => return UserDefinedInvoke(Err(e))
                    };
                    UserDefinedInvoke(Ok((Some(*body), active_env)))
                },
                _ => return UserDefinedInvoke(Err("eval: proc invoke: should've \
                                gotten a list in the cdr".to_string()))
            }
        },
        Atom(Lambda(BuiltIn(_, _body_fn))) => {
            match cdr {
                list @ List(_) => {
                    let (args, env) = match list.unbox_and_eval(env) {
                        Ok(r) => r,
                        Err(e) => return BuiltInInvoke(Err(
                            format!("eval(): error in unbox: {}", e)))
                    };
                    let result = match _body_fn(args, env) {
                        Ok(r) => r,
                        Err(e) => return BuiltInInvoke(Err(e))
                    };
                    BuiltInInvoke(Ok(result))
                },
                _ => return BuiltInInvoke(Err("cdr should be List(...)".to_string()))
            }
        }, _ => return BuiltInInvoke(Err(format!("defined var {} is not a procedure!", proc_name)))
    }
}
