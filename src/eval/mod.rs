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
pub type TrampolineResult = SchemerResult<(Option<Expr>, Option<Env>)>;

mod begin;
mod define;
mod if_impl;
mod invoke;
mod lambda;
mod quote;
mod set;

pub fn eval<'env>(expr: Expr, in_env: Env) -> EvalResult {
    let out_expr = None;
    let mut out_env: Option<Env> = None;
    let mut in_expr = Some(expr);
    loop {
        let env = match out_env {
            Some(ref e) => e.clone(),
            None => in_env.clone()
        };
        match in_expr.clone() {
        None => break,
        Some(expr) => match expr {
            // Atom values and values in the Env
            Atom(Symbol(ref var_name)) => {
                let out_val = try!(env.find(var_name));
                return Ok((Some(out_val.clone()), env))
            },
            // Literal value, return it outright
            val @ Atom(_) => return Ok((Some(val), env)),
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
                        return quote::eval_quote(cdr, env)
                    },
                    // (set! var-name expr)
                    Atom(ref val) if *val == Symbol("set!".to_string()) => {
                        return set::eval_set(cdr, env)
                    },
                    // (define var-name expr)
                    Atom(ref val) if *val == Symbol("define".to_string()) => {
                        let out_env = try!(define::eval_define(cdr, env));
                        return Ok((None, out_env))
                    },
                    // (lamda (args) expr)
                    Atom(ref val) if *val == Symbol("lambda".to_string()) => {
                        let eval_result = try!(lambda::eval_lambda(
                                "anonymous".to_string(), cdr, env.clone()));
                        return Ok((eval_result, env))
                    },
                    // (if (test) (conseq) (alt))
                    Atom(ref val) if *val == Symbol("if".to_string()) => {
                        return if_impl::eval_if(cdr, env)
                    },
                    // (begin (expr) [..(expr)])
                    Atom(ref val) if *val == Symbol("begin".to_string()) => {
                        match begin::eval_begin(cdr, env) {
                            Ok((expr, env)) => { in_expr = expr; out_env = env; },
                            Err(e) => return Err(e)
                        }
                    },
                    // oh boy a procedure call!
                    Atom(Symbol(proc_name)) => {
                        return invoke::eval_invoke(proc_name, cdr, env)
                    },
                    // otherwise an Err result
                    car => return Err(
    format!("car of list isn't a symbol for invocation lookup. Value: {}", car))
                }
            }
        }}
    }
    let out_env = match out_env {
        Some(e) => e,
        None => return Err("eval: got a None in out_env.. shouldn't happen".to_string())
    };
    Ok((out_expr, out_env))
}
