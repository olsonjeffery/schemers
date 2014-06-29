// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, Atom, List, Symbol};
use env::Env;
use result::SchemerResult;

pub type EvalResult = SchemerResult<(Option<Expr>, Env)>;

mod begin;
mod define;
mod if_impl;
mod invoke;
mod lambda;
mod quote;
mod set;

pub fn eval<'env>(expr: Expr, env: Env) -> EvalResult {
    match expr {
        // Atom values and values in the Env
        Atom(Symbol(ref var_name)) => {
            let out_val = try!(env.find(var_name));
            Ok((Some(out_val.clone()), env))
        },
        // Literal value, return it outright
        val @ Atom(_) => Ok((Some(val), env)),
        // Some kind of list expr
        list @ List(_) => {
            // Empty list, return as-is
            if list.is_null() {
                return Ok((Some(list), env));
            }
            let (car, cdr) = try!(list.un_cons());
            match car {
                // (quote expr)
                Atom(ref val) if *val == Symbol("quote".to_string())  => {
                    quote::eval_quote(cdr, env)
                },
                // (if (test) (conseq) (alt))
                Atom(ref val) if *val == Symbol("if".to_string()) => {
                    if_impl::eval_if(cdr, env)
                },
                Atom(ref val) if *val == Symbol("set!".to_string()) => {
                    set::eval_set(cdr, env)
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
                    invoke::eval_invoke(proc_name, cdr, env)
                }, _ => return Err("car of list isn't a symbol for invocation lookup".to_string())
            }
        }
    }
}
