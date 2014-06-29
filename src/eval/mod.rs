// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, Atom, List, Symbol, Lambda, UserDefined, BuiltIn};
use env::Env;
use result::SchemerResult;

pub type EvalResult = SchemerResult<(Option<Expr>, Env)>;

mod begin;
mod define;
mod if_impl;
mod lambda;

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval<'env>(expr: Expr, env: Env) -> EvalResult {
    match expr {
        // Atom values and values in the Env
        Atom(Symbol(ref var_name)) => {
            let out_val = try!(env.find(var_name));
            Ok((Some(out_val.clone()), env))
        },
        val @ Atom(_) => Ok((Some(val), env)),
        list @ List(_) => {
            if list.is_null() {
                return Ok((Some(list), env));
            }
            let (car, cdr) = try!(list.un_cons());
            match car {
                Atom(ref val) if *val == Symbol("quote".to_string())  => {
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
                },
                Atom(ref val) if *val == Symbol("if".to_string()) => {
                    if_impl::eval_if(cdr, env)
                },
                Atom(ref val) if *val == Symbol("set!".to_string()) => {
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
                },
                Atom(ref val) if *val == Symbol("define".to_string()) => {
                    let out_env = try!(define::eval_define(cdr, env));
                    Ok((None, out_env))
                },
                Atom(ref val) if *val == Symbol("lambda".to_string()) => {
                    let eval_result = try!(lambda::eval_lambda(
                            "anonymous".to_string(), cdr, env.clone()));
                    Ok((eval_result, env))
                },
                Atom(ref val) if *val == Symbol("begin".to_string()) => {
                    begin::eval_begin(cdr, env)
                },
                // oh boy a procedure call!
                Atom(Symbol(proc_name)) => {
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
                }, _ => return Err("car of list isn't a symbol for invocation lookup".to_string())
            }
        }
    }
}
