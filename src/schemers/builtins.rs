// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use num::bigint::BigInt;
use num::rational::BigRational;
use env::Env;
use expr::{Expr, Atom, Lambda, BuiltIn, Integer, Float, Symbol,
           Boolean, List, Number};
use result::SchemerResult;

pub fn add_builtins(mut env: Env) -> Env {
    env.define("+".to_string(), Atom(Lambda(BuiltIn("+".to_string(), builtin_add))));
    env.define("-".to_string(), Atom(Lambda(BuiltIn("-".to_string(), builtin_subtract))));
    env.define("*".to_string(), Atom(Lambda(BuiltIn("*".to_string(), builtin_multiply))));
    env.define("/".to_string(), Atom(Lambda(BuiltIn("/".to_string(), builtin_divide))));
    env.define("<".to_string(), Atom(Lambda(BuiltIn("<".to_string(), builtin_lt))));
    env.define(">".to_string(), Atom(Lambda(BuiltIn(">".to_string(), builtin_gt))));
    env.define("<=".to_string(), Atom(Lambda(BuiltIn("<=".to_string(), builtin_lte))));
    env.define(">=".to_string(), Atom(Lambda(BuiltIn(">=".to_string(), builtin_gte))));
    env.define("not".to_string(), Atom(Lambda(BuiltIn("not".to_string(), builtin_not))));
    env.define("=".to_string(), Atom(Lambda(BuiltIn("=".to_string(), builtin_eq))));
    env.define("eq?".to_string(), Atom(Lambda(BuiltIn("eq?".to_string(), builtin_eq))));
    env.define("equal?".to_string(), Atom(Lambda(BuiltIn("equal?".to_string(), builtin_eq))));
    env.define("length".to_string(), Atom(Lambda(BuiltIn("length".to_string(), builtin_length))));
    env.define("cons".to_string(), Atom(Lambda(BuiltIn("cons".to_string(), builtin_cons))));
    env.define("car".to_string(), Atom(Lambda(BuiltIn("car".to_string(), builtin_car))));
    env.define("cdr".to_string(), Atom(Lambda(BuiltIn("cdr".to_string(), builtin_cdr))));
    env.define("append".to_string(), Atom(Lambda(BuiltIn("append".to_string(), builtin_append))));
    env.define("list".to_string(), Atom(Lambda(BuiltIn("list".to_string(), builtin_list))));
    env.define("list?".to_string(), Atom(Lambda(BuiltIn("list?".to_string(), builtin_is_list))));
    env.define("null?".to_string(), Atom(Lambda(BuiltIn("null?".to_string(), builtin_is_null))));
    env.define("symbol?".to_string(),
               Atom(Lambda(BuiltIn("symbol?".to_string(), builtin_is_symbol))));
    env.define("display".to_string(),
               Atom(Lambda(BuiltIn("display".to_string(), builtin_display))));
    env
}

fn builtin_arithmetic(mut args: Vec<Expr>, env: Env,
                      name: &str,
                      transformer_bi: |BigInt, BigInt| -> BigInt,
                      transformer_br: |BigRational, BigRational| -> BigRational,
                      ) -> SchemerResult<(Option<Expr>, Env)> {
    let mut hasFloats = false;
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => return Err(format!("{}: invoking with non numeric input: '{}'",
                         name, val.print()))
        }
    }
    let out_val = if hasFloats {
        let mut out_val = match args.shift()
            {
                Some(v) => match v {
                    v @ Atom(Integer(_)) | v @ Atom(Float(_)) => match v.into_float() {
                        Ok(v) => v,
                        Err(e) => return Err(format!("{}: {}", name, e))
                    },
                    _ => return Err(format!("{}: cannot process non-numeric input", name))
                },
                None => return Err(format!("{}: head of add args should be Some()", name)) 
            };
        for atom in args.move_iter() {
            match atom {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) =>
                    out_val = match v.into_float() {
                        Ok(v) => transformer_br(out_val, v),
                        Err(e) => return Err(format!("{}: {}", name, e))
                    },
                _ => return Err(format!("{}: invoking with non numeric input", name))
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = match args.shift()
            .expect("head of add args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => match v.into_integer() {
                    Ok(v) => v,
                    Err(e) => return Err(format!("{}", e))
                },
                _ => return Err(format!("{}: cannot process non-numeric input", name))
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(val)) => out_val = transformer_bi(out_val, val),
                _ => return Err(format!("{}: invoking with non numeric input", name))
            }
        }
        Atom(Integer(out_val))
    };
    Ok((Some(out_val), env))
}

fn builtin_add(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = "+";
    let transformer_bi = |left:BigInt, right: BigInt| left + right;
    let transformer_br = |left:BigRational, right: BigRational| left + right;
    match builtin_arithmetic(args, env, name, transformer_bi, transformer_br) {
        Ok(v) => v,
        Err(e) => fail!("{}: {}", name, e)
    }
}

fn builtin_subtract(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = "-";
    let transformer_bi = |left:BigInt, right: BigInt| left - right;
    let transformer_br = |left:BigRational, right: BigRational| left - right;
    match builtin_arithmetic(args, env, name, transformer_bi, transformer_br) {
        Ok(v) => v,
        Err(e) => fail!("{}: {}", name, e)
    }
}

fn builtin_multiply(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = "*";
    let transformer_bi = |left:BigInt, right: BigInt| left * right;
    let transformer_br = |left:BigRational, right: BigRational| left * right;
    match builtin_arithmetic(args, env, name, transformer_bi, transformer_br) {
        Ok(v) => v,
        Err(e) => fail!("{}: {}", name, e)
    }
}

fn builtin_divide(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = "/";
    let transformer_bi = |left:BigInt, right: BigInt| left / right;
    let transformer_br = |left:BigRational, right: BigRational| left / right;
    match builtin_arithmetic(args, env, name, transformer_bi, transformer_br) {
        Ok(v) => v,
        Err(e) => fail!("{}: {}", name, e)
    }
}

fn builtin_predicate_compare(mut args: Vec<Expr>,
                   env: Env,
                   name: &str,
                   transformer_bi: |BigInt, BigInt| -> bool,
                   transformer_br: |BigRational, BigRational| -> bool,
                   ) -> SchemerResult<(Option<Expr>, Env)> {
    let mut hasFloats = false;
    if args.len() < 2 {
        return Err(format!("{}: too few parameters (must provide at least two)", name));
    }
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => return Err(format!(
                "{}: invoking with non numeric input: '{}'", name, val.print()))
        }
    }
    let out_val = if hasFloats {
        let mut state = false;
        let mut left = match args.shift() {
            None => return Err(format!("{}: head of less-than args should be Some()", name)),
            Some(v) => match v {
                    left @ Atom(Integer(_)) |
                    left @ Atom(Float(_)) => try!(left.into_float()),
                    _ => return Err(format!("{}: no match", name))
                }
            };
        for atom in args.move_iter() {
            match atom {
                right @ Atom(Integer(_)) | right @ Atom(Float(_)) => {
                    let right = try!(right.into_float());
                    state = transformer_br(left, right.clone());
                    left = right;
                },
                _ => return Err(format!("{}: invoking with non numeric input", name))
            }
        }
        Atom(Boolean(state))
    } else {
        let mut state = false;
        let mut left = match args.shift() {
            None => return Err(format!("{}: head of less-than args should be Some()", name)),
            Some(v) => match v {
                left @ Atom(Integer(_))
                    | left @ Atom(Float(_)) => try!(left.into_integer()),
                _ => return Err(format!("{}: cannot process non-numeric input", name))
            }
        };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(right)) => {
                    state = transformer_bi(left, right.clone());
                    left = right;
                }
                _ => return Err(format!("{}: invoking with non numeric input", name))
            }
        }
        Atom(Boolean(state))
    };
    Ok((Some(out_val), env))
}

fn builtin_lt(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = "<";
    let transformer_bi = |left:BigInt, right:BigInt| left < right;
    let transformer_br = |left:BigRational, right:BigRational| left < right;
    match builtin_predicate_compare(args, env, name, transformer_bi,
                          transformer_br) {
        Ok(r) => r,
        Err(e) => fail!("{}", e)
    }
}

fn builtin_gt(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = ">";
    let transformer_bi = |left:BigInt, right:BigInt| left > right;
    let transformer_br = |left:BigRational, right:BigRational| left > right;
    match builtin_predicate_compare(args, env, name, transformer_bi,
                          transformer_br) {
        Ok(r) => r,
        Err(e) => fail!("{}", e)
    }
}

fn builtin_lte(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = "<=";
    let transformer_bi = |left:BigInt, right:BigInt| left <= right;
    let transformer_br = |left:BigRational, right:BigRational| left <= right;
    match builtin_predicate_compare(args, env, name, transformer_bi,
                          transformer_br) {
        Ok(r) => r,
        Err(e) => fail!("{}", e)
    }
}

fn builtin_gte(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let name = ">=";
    let transformer_bi = |left:BigInt, right:BigInt| left >= right;
    let transformer_br = |left:BigRational, right:BigRational| left >= right;
    match builtin_predicate_compare(args, env, name, transformer_bi,
                          transformer_br) {
        Ok(r) => r,
        Err(e) => fail!("{}", e)
    }
}

fn builtin_not(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("not: expect a single parameter")
    }
    match args.pop().unwrap() {
        Atom(Boolean(false)) => (Some(Atom(Boolean(true))), env),
        _ => (Some(Atom(Boolean(false))), env),
    }
}
fn builtin_eq(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() < 2 {
        fail!("eq: expect at least two parameters")
    }
    let mut state = false;
    let mut left = args.shift().expect("head of eq args should be Some()");
    for right in args.move_iter() {
        state = left == right;
        left = right;
    }
    (Some(Atom(Boolean(state))), env)
}
fn builtin_length(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("length: expects only a single list parameter")
    }
    match args.shift().expect("head of length args should be Some()") {
        List(items) => {
            println!("length: {}", items.len());
            (Some(Atom(Number::uint(items.len()))), env)
        }
        _ => fail!("length: expecting a list parameter")
    }
}
fn builtin_cons(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 2 {
        fail!("cons: expects two parameters")
    }
    let cons_result = match Expr::cons(args.shift().unwrap(), args.shift().unwrap()) {
        Ok(r) => r,
        Err(e) => fail!("builtin_cons: failure in Expr::cons(): {}", e)
    };
    (Some(cons_result), env)
}
fn builtin_car(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("car: expect a single list parameter");
    }
    let (car, _) = match args.pop().unwrap() {
        v @ List(_) => match v.un_cons() {
            Ok(v) => v,
            Err(e) => fail!("car: {}", e)
        },
        _ => fail!("car: expected a list")
    };
    (Some(car), env)
}
fn builtin_cdr(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("cdr: expect a single list parameter");
    }
    let (_, cdr) = match args.pop().unwrap() {
        v @ List(_) => match v.un_cons() {
            Ok(v) => v,
            Err(e) => fail!("cdr: {}", e)
        },
        _ => fail!("cdr: expected a list")
    };
    (Some(cdr), env)
}
fn builtin_append(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 2 {
        fail!("append: expect two arguments");
    }
    let out_items = match args.shift().unwrap() {
        List(mut items) => {
            items.push(box args.pop().unwrap());
            items
        },
        _ => fail!("append: expect first arg to be a list")
    };
    (Some(List(out_items)), env)
}
fn builtin_list(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    (Some(List(args.move_iter().map(|x| box x).collect())), env)
}
fn builtin_is_list(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("list?: expect a single argument");
    }
    let result = match args.shift().unwrap() {
        List(_) => {
            Atom(Boolean(true))
        },
        _ => Atom(Boolean(false))
    };
    (Some(result), env)
}

fn builtin_is_null(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("null?: expect a single argument");
    }
    let result = match args.shift().unwrap() {
        List(ref v) if v.len() == 0 => {
            Atom(Boolean(true))
        },
        _ => Atom(Boolean(false))
    };
    (Some(result), env)
}

fn builtin_is_symbol(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("symbol?: expect a single argument");
    }
    let result = match args.shift().unwrap() {
        Atom(Symbol(_)) => {
            Atom(Boolean(true))
        },
        _ => Atom(Boolean(false))
    };
    (Some(result), env)
}

fn builtin_display(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("display: expect a single argument");
    }
    println!("{}", args.shift().unwrap().print());
    (None, env)
}
