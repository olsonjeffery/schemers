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
pub mod if_impl;

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
                    let out_env = try!(eval_define(cdr, env));
                    Ok((None, out_env))
                },
                Atom(ref val) if *val == Symbol("lambda".to_string()) => {
                    let eval_result = try!(eval_lambda("anonymous".to_string(), cdr, env.clone()));
                    Ok((eval_result, env))
                },
                Atom(ref val) if *val == Symbol("begin".to_string()) => {
                    eval_begin(cdr, env)
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

fn eval_define(cdr: Expr, env: Env) -> SchemerResult<Env> {
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
                                            eval_lambda(name.to_string(), cdr, env.clone()));
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

fn eval_lambda(name: String, cdr: Expr, env: Env) -> SchemerResult<Option<Expr>> {
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

fn eval_begin(cdr: Expr, env: Env) -> EvalResult {
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
