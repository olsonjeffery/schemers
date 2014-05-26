// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
#![crate_id="http://olsonjeffery.github.io#schemers:0.0.1"]
#![desc = "Simple Scheme Interpreter"]
#![license = "3-Clause BSD"]

extern crate collections = "collections";
extern crate num = "num";

use std::from_str::FromStr;
use std::fmt::{Show, Formatter, Result};
use collections::hashmap::HashMap;
use std::num::One;
use num::rational::{Ratio, BigRational};
use num::bigint::BigInt;

// this is starting out as a straight port of Peter Norvig's
// lis.py (the first iteration) to Rust

#[cfg(not(test))]
fn main() {
    use std::os::args;
    let args: Vec<~str> = args().move_iter().skip(1).collect();
    if args.len() == 0 {
        fail!("no program provided");
    }
    let program: ~str = args.move_iter()
        .fold(~"", |memo, arg| memo + arg + " ").trim().to_owned();
    let env = add_builtins(Env::new(None, None, None));
    match eval(read(program), env) {
        (Some(expr), _) => println!("=> {}", expr.print()),
        _ => {}
    }
}

// Language data
pub struct Env {
    entries: HashMap<~str, Expr>,
    outer: Option<~Env>
}

#[deriving(Eq, Show, Clone)]
pub enum Expr {
    Atom(AtomVal),
    List(Vec<~Expr>)
}

#[deriving(Eq, Show, Clone)]
pub enum AtomVal {
    Symbol(~str),
    Integer(BigInt),
    Float(BigRational),
    Lambda(LambdaVal),
    Boolean(bool)
}

#[deriving(Clone)]
pub enum LambdaVal {
    UserDefined(~str, Vec<~str>, ~Expr),
    BuiltIn(~str, fn(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env))
}

impl LambdaVal {
    pub fn print(&self) -> ~str {
        match self {
            &UserDefined(ref name, ref vars, _) => {
                let var_expr = List(vars.iter().map(|v| ~Atom(Symbol(v.to_owned()))).collect());
                format!("lambda:{}{}", name.to_owned(), var_expr.print())
            },
            &BuiltIn(ref name, _) =>
                format!("builtin-fn:{}", name.to_owned())
        }
    }
}

impl Eq for LambdaVal {
    // Our custom eq allows numbers which are near each other to be equal! :D
    fn eq(&self, other: &LambdaVal) -> bool {
        match self {
            &UserDefined(ref name, ref args, ref body) => match other {
                &UserDefined(ref other_name, ref other_args, ref other_body) =>
                    name == other_name &&
                {
                    if args.len() != other_args.len() {
                        return false;
                    }
                    let mut args_match = true;
                    for ctr in range(0, args.len()) {
                        args_match = args.get(ctr)
                            == other_args.get(ctr);
                    }
                    args_match
                } &&
                    body == other_body,
                &BuiltIn(_, _) => false
            },
            &BuiltIn(ref name, ref body_fn) => match other {
                &BuiltIn(ref other_name, ref other_body_fn) =>
                    *body_fn as *u8 == *other_body_fn as *u8 && name == other_name,
                &UserDefined(_, _, _) => false
            }
        }
    }
}

impl Show for LambdaVal {
    fn fmt(&self, f: &mut Formatter) -> Result {
        // The `f.buf` value is of the type `&mut io::Writer`, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        write!(f.buf, "{}", self.print())
    }
}

// eval'ing Expr->Expr impl
fn eval<'env>(expr: Expr, env: Env) -> (Option<Expr>, Env) {
    match expr {
        // Atom values and values in the Env
        Atom(Symbol(ref var_name)) => {
            let out_val = env.find(var_name);
            (Some(out_val.clone()), env)
        },
        val @ Atom(_) => (Some(val), env),
        list @ List(_) => {
            if list.is_null() {
                return (Some(list), env);
            }
            let (car, cdr) = list.un_cons();
            match car {
                Atom(ref val) if *val == Symbol(~"quote")  => {
                    match cdr {
                        List(mut items) =>
                            (Some(*items.shift().expect("eval: quote list shouldnt be empty")),
                             env),
                        _ => fail!("eval: quote: expected List in cdr position")
                    }
                },
                Atom(ref val) if *val == Symbol(~"if") => {
                    eval_if(cdr, env)
                },
                Atom(ref val) if *val == Symbol(~"set!") => {
                    match cdr {
                        List(mut items) => {
                            if items.len() != 2 {
                                fail!("eval: set!: expected two entries");
                            }
                            match *items.shift().expect("eval: set!: var name should have value") {
                                Atom(Symbol(name)) => {
                                    let (val_expr, mut env) =
                                        eval(*items.pop()
                                             .expect("eval: set! val not provided"), env);
                                    env.set(name,
                                        val_expr.expect(
                                            "eval: set!: provided val didn't resolve"));
                                    (None, env)
                                },
                                _ => fail!("eval: set!: atom in var name position must be symbol")
                            }
                        },
                        _ => fail!("eval: set!: expected list in cdr position")
                    }
                },
                Atom(ref val) if *val == Symbol(~"define") => {
                    (None, eval_define(cdr, env))
                },
                Atom(ref val) if *val == Symbol(~"lambda") => {
                    (eval_lambda(~"anonymous", cdr), env)
                },
                Atom(ref val) if *val == Symbol(~"begin") => {
                    eval_begin(cdr, env)
                },
                // oh boy a procedure call!
                Atom(Symbol(proc_name)) => {
                    let target_proc = env.find(&proc_name);
                    match target_proc {
                        Atom(Lambda(UserDefined(_, vars, body))) => {
                            match cdr {
                                args @ List(_) => {
                                    let (args, env) = args.unbox_and_eval(env);
                                    let new_env = Env::new(Some(vars),
                                                           Some(args), Some(env));
                                    let (out_expr, new_env) = eval(*body, new_env);
                                    (out_expr, new_env.unwrap_parent())
                                },
                                _ => fail!("eval: proc invoke: should've gotten a list in the cdr")
                            }
                        },
                        Atom(Lambda(BuiltIn(_, _body_fn))) => {
                            match cdr {
                                list @ List(_) => {
                                    let (args, env) = list.unbox_and_eval(env);
                                    _body_fn(args, env)
                                },
                                _ => fail!("expected cdr to be List(...)")
                            }
                        }, _ => fail!("defined var {} is not a procedure!", proc_name)
                    }
                }, _ => fail!("car of list isn't a symbol for invocation lookup")
            }
        }
    }
}

fn eval_define(cdr: Expr, env: Env) -> Env {
    match cdr {
        List(mut items) => {
            if items.len() != 2 {
                fail!("eval: define: expected two entries");
            }
            match *items.shift()
                .expect("eval: define: var name should have value") {
                    Atom(Symbol(name)) => {
                        let val_expr = *items.pop().expect("eval: define: value should be there");
                        let (val_expr, mut env) = {
                            if val_expr.is_atom() {
                                eval(val_expr , env)
                            } else {
                                let (car, cdr) = val_expr.clone().un_cons();
                                match car {
                                    Atom(ref val) if *val == Symbol(~"lambda") => {
                                        (eval_lambda(name.to_owned(), cdr), env)
                                    }
                                    _ => eval(val_expr , env)
                                }
                            }
                        };
                        env.define(name,
                                   val_expr.expect(
                                       "eval: define: val expr should return something"));
                        env
                    },
                    _ => fail!("eval: define: atom in var pos. must be symbol")
                }
        },
        _ => fail!("eval: define: expected list in cdr position")
    }
}

fn eval_lambda(name: ~str, cdr: Expr) -> Option<Expr> {
    match cdr {
        List(mut items) => {
            if items.len() != 2 {
                fail!("eval: lambda: expected two entries in cdr");
            }
            let mut var_names = Vec::new();
            match *items.shift().expect("eval: lambda: vars should be there") {
                List(vars) => {
                    for v in vars.iter() {
                        match v {
                            &~Atom(Symbol(ref var_name)) => {
                                var_names.push(var_name.to_owned());
                            },
                            _ => fail!("eval: lambda: var names must be symbols")
                        }
                    }
                }, _ => fail!("eval: lambda: expect vars to be list")
            }
            Some(Atom(Lambda(UserDefined(name, var_names, items.shift()
                                         .expect("eval: lambda: lambda body should be there")))))
        },
        _ => fail!("eval: lambda: expected list in cdr position")
    }
}

fn eval_begin(cdr: Expr, env: Env) -> (Option<Expr>, Env) {
    match cdr {
        List(items) => {
            let (mut out_expr, mut env) = (None, env);
            for e in items.move_iter() {
                let (res_expr, res_env) = eval(*e, env);
                out_expr = res_expr;
                env = res_env;
            }
            (out_expr, env)
        },
        _ => fail!("eval: begin: expcted a list for the input cdr")
    }
}

fn eval_if(cdr: Expr, env: Env) -> (Option<Expr>, Env) {
    match cdr {
        List(mut items) => {
            if items.len() != 3 {
                fail!("eval: if: expect three entries in if cdr list");
            }
            let (out_expr, out_env) = eval(
                *items.shift().unwrap(), env);
            match out_expr {
                Some(Atom(Boolean(false))) => {
                    eval(*items.pop().unwrap(), out_env)
                },
                _ => eval(*items.shift().unwrap(), out_env)
            }
        },
        _ => fail!("eval: if: expected List in cdr position")
    }
}

// standard procedures impl
fn add_builtins(mut env: Env) -> Env {
    env.define(~"+", Atom(Lambda(BuiltIn(~"+", builtin_add))));
    env.define(~"-", Atom(Lambda(BuiltIn(~"-", builtin_subtract))));
    env.define(~"*", Atom(Lambda(BuiltIn(~"*", builtin_multiply))));
    env.define(~"/", Atom(Lambda(BuiltIn(~"/", builtin_divide))));
    env.define(~"<", Atom(Lambda(BuiltIn(~"<", builtin_lt))));
    env.define(~">", Atom(Lambda(BuiltIn(~">", builtin_gt))));
    env.define(~"<=", Atom(Lambda(BuiltIn(~"<=", builtin_lte))));
    env.define(~">=", Atom(Lambda(BuiltIn(~">=", builtin_gte))));
    env.define(~"not", Atom(Lambda(BuiltIn(~"not", builtin_not))));
    env.define(~"=", Atom(Lambda(BuiltIn(~"=", builtin_eq))));
    env.define(~"eq?", Atom(Lambda(BuiltIn(~"eq?", builtin_eq))));
    env.define(~"equal?", Atom(Lambda(BuiltIn(~"equal?", builtin_eq))));
    env.define(~"length", Atom(Lambda(BuiltIn(~"length", builtin_length))));
    env.define(~"cons", Atom(Lambda(BuiltIn(~"cons", builtin_cons))));
    env.define(~"car", Atom(Lambda(BuiltIn(~"car", builtin_car))));
    env.define(~"cdr", Atom(Lambda(BuiltIn(~"cdr", builtin_cdr))));
    env.define(~"append", Atom(Lambda(BuiltIn(~"append", builtin_append))));
    env.define(~"list", Atom(Lambda(BuiltIn(~"list", builtin_list))));
    env.define(~"list?", Atom(Lambda(BuiltIn(~"list?", builtin_is_list))));
    env.define(~"null?", Atom(Lambda(BuiltIn(~"null?", builtin_is_null))));
    env.define(~"symbol?", Atom(Lambda(BuiltIn(~"symbol?", builtin_is_symbol))));
    env
}
fn builtin_add(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("add: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut out_val = match args.shift()
            .expect("head of add args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_float(),
                _ => fail!("add: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) =>
                    out_val = out_val + v.unwrap_float(),
                _ => fail!("add: invoking with non numeric input")
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = match args.shift()
            .expect("head of add args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_integer(),
                _ => fail!("add: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(val)) => out_val = out_val + val,
                _ => fail!("add: invoking with non numeric input")
            }
        }
        Atom(Integer(out_val))
    };
    (Some(out_val), env)
}

fn builtin_subtract(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("subtract: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut out_val = match args.shift()
            .expect("head of subtract args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_float(),
                _ => fail!("subtract: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) =>
                    out_val = out_val - v.unwrap_float(),
                _ => fail!("subtract: invoking with non numeric input")
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = match args.shift()
            .expect("head of subtract args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_integer(),
                _ => fail!("subtract: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(val)) => out_val = out_val - val,
                _ => fail!("subtract: invoking with non numeric input")
            }
        }
        Atom(Integer(out_val))
    };
    (Some(out_val), env)
}

fn builtin_multiply(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("multiply: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut out_val = match args.shift()
            .expect("head of multiply args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_float(),
                _ => fail!("multiply: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) =>
                    out_val = out_val * v.unwrap_float(),
                _ => fail!("multiply: invoking with non numeric input")
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = match args.shift()
            .expect("head of multiply args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_integer(),
                _ => fail!("multiply: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(val)) => out_val = out_val * val,
                _ => fail!("multiply: invoking with non numeric input")
            }
        }
        Atom(Integer(out_val))
    };
    (Some(out_val), env)
}

fn builtin_divide(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("divide: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut out_val = match args.shift()
            .expect("head of divide args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_float(),
                _ => fail!("divide: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) =>
                    out_val = out_val / v.unwrap_float(),
                _ => fail!("divide: invoking with non numeric input")
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = match args.shift()
            .expect("head of divide args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_integer(),
                _ => fail!("divide: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(val)) => out_val = out_val / val,
                _ => fail!("divide: invoking with non numeric input")
            }
        }
        Atom(Integer(out_val))
    };
    (Some(out_val), env)
}

fn builtin_lt(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    if args.len() < 2 {
        fail!("<: too few parameters (must provide at least two)");
    }
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("less-than: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of less-than args should be Some()") {
                left @ Atom(Integer(_)) |
                left @ Atom(Float(_)) => left.unwrap_float(),
                _ => fail!("less-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                right @ Atom(Integer(_)) | right @ Atom(Float(_)) => {
                    let right = right.unwrap_float();
                    state = left < right;
                    left = right;
                },
                _ => fail!("less-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    } else {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of less-than args should be Some()") {
                left @ Atom(Integer(_))
                    | left @ Atom(Float(_)) => left.unwrap_integer(),
                _ => fail!("less-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(right)) => {
                    state = left < right;
                    left = right;
                }
                _ => fail!("less-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    };
    (Some(out_val), env)
}

fn builtin_gt(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    if args.len() < 2 {
        fail!("<: too few parameters (must provide at least two)");
    }
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("greater-than: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of greater-than args should be Some()") {
                left @ Atom(Integer(_)) |
                left @ Atom(Float(_)) => left.unwrap_float(),
                _ => fail!("greater-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                right @ Atom(Integer(_)) | right @ Atom(Float(_)) => {
                    let right = right.unwrap_float();
                    state = left > right;
                    left = right;
                },
                _ => fail!("greater-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    } else {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of greater-than args should be Some()") {
                left @ Atom(Integer(_))
                    | left @ Atom(Float(_)) => left.unwrap_integer(),
                _ => fail!("greater-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(right)) => {
                    state = left > right;
                    left = right;
                }
                _ => fail!("greater-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    };
    (Some(out_val), env)
}

fn builtin_lte(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    if args.len() < 2 {
        fail!("<: too few parameters (must provide at least two)");
    }
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("less-than: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of less-than args should be Some()") {
                left @ Atom(Integer(_)) |
                left @ Atom(Float(_)) => left.unwrap_float(),
                _ => fail!("less-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                right @ Atom(Integer(_)) | right @ Atom(Float(_)) => {
                    let right = right.unwrap_float();
                    state = left <= right;
                    left = right;
                },
                _ => fail!("less-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    } else {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of less-than args should be Some()") {
                left @ Atom(Integer(_))
                    | left @ Atom(Float(_)) => left.unwrap_integer(),
                _ => fail!("less-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(right)) => {
                    state = left <= right;
                    left = right;
                }
                _ => fail!("less-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    };
    (Some(out_val), env)
}

fn builtin_gte(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    let mut hasFloats = false;
    if args.len() < 2 {
        fail!("<: too few parameters (must provide at least two)");
    }
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("greater-than: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of greater-than args should be Some()") {
                left @ Atom(Integer(_)) |
                left @ Atom(Float(_)) => left.unwrap_float(),
                _ => fail!("greater-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                right @ Atom(Integer(_)) | right @ Atom(Float(_)) => {
                    let right = right.unwrap_float();
                    state = left >= right;
                    left = right;
                },
                _ => fail!("greater-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    } else {
        let mut state = false;
        let mut left = match args.shift()
            .expect("head of greater-than args should be Some()") {
                left @ Atom(Integer(_))
                    | left @ Atom(Float(_)) => left.unwrap_integer(),
                _ => fail!("greater-than: cannot process non-numeric input")
            };
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(right)) => {
                    state = left >= right;
                    left = right;
                }
                _ => fail!("greater-than: invoking with non numeric input")
            }
        }
        Atom(Boolean(state))
    };
    (Some(out_val), env)
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
    if args.len() > 1 {
        fail!("length: expects only a single list parameter")
    }
    match args.shift().expect("head of length args should be Some()") {
        List(items) => {
            (Some(Atom(Number::uint(items.len()))), env)
        }
        _ => fail!("length: expecting a list parameter")
    }
}
fn builtin_cons(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 2 {
        fail!("cons: expects two parameters")
    }
    (Some(Expr::cons(args.shift().unwrap(), args.shift().unwrap())), env)
}
fn builtin_car(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("car: expect a single list parameter");
    }
    let (car, _) = match args.pop().unwrap() {
        v @ List(_) => v.un_cons(),
        _ => fail!("car: expected a list")
    };
    (Some(car), env)
}
fn builtin_cdr(mut args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    if args.len() != 1 {
        fail!("cdr: expect a single list parameter");
    }
    let (_, cdr) = match args.pop().unwrap() {
        v @ List(_) => v.un_cons(),
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
            items.push(~args.pop().unwrap());
            items
        },
        _ => fail!("append: expect first arg to be a list")
    };
    (Some(List(out_items)), env)
}
fn builtin_list(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    (Some(List(args.move_iter().map(|x| ~x).collect())), env)
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

// parser impl
fn read(input: ~str) -> Expr {
    parse(&mut tokenize(input))
}

fn pad_input(input: ~str) -> ~str {
    input.replace("(", " ( ").replace(")", " ) ")
}

fn strip_commas(input: ~str) -> ~str {
    input.replace(",", " ").replace("  ", " ")
}

fn tokenize(input: ~str) -> Vec<~str> {
    strip_commas(pad_input(input)).split_str(" ")
        .filter(|i| *i != "").map(|i| i.to_owned()).collect()
}

fn parse(tokens: &mut Vec<~str>) -> Expr {
    let current_token =
        tokens.shift().expect("calling parse() w/ empty token list; shouldn't happen");
    match current_token {
        ref x if *x == ~"(" => {
            let mut list = Vec::new();
            while *tokens.get(0) != ~")" {
                list.push(box parse(tokens));
            }
            tokens.shift();
            List(list)
        },
        ref x if *x == ~")" => fail!("hit ) token; shouldn't happen"),
        x => Expr::new_atom(x)
    }
}

impl Env {
    pub fn new(
        params: Option<Vec<~str>>,
        args: Option<Vec<Expr>>,
        outer: Option<Env>) -> Env {
        let mut entries = HashMap::new();
        if params.is_some() && args.is_some() {
            let params = params.expect("params should be a value");
            let args = args.expect("args should be a value");
            if params.len() == args.len() {
                for ctr in range(0,params.len()) {
                    let var_name = params.get(ctr).to_owned();
                    let arg = args.get(ctr).clone();
                    entries.insert(var_name, arg);
                }
            } else {
                fail!("params and args length doesn't match")
            }
        }
        else if params.is_none() && args.is_none() {
            // do nothing!
        } else {
            fail!("cannot have params & args unset")
        }
        Env { entries: entries, outer: match outer { Some(e) => Some(~e), None => None } }
    }

    pub fn set(&mut self, symbol: ~str, val: Expr) {
        match self.entries.contains_key(&symbol) {
            true => {
                self.define(symbol, val);
            },
            false => match self.outer {
                Some(ref mut outer_env) => outer_env.set(symbol, val),
                None => fail!("Symbol '{}' is not defined in this Env scope chain",
                              symbol)
            }
        }
    }

    pub fn define(&mut self, symbol: ~str, val: Expr) {
        self.entries.insert(symbol, val);
    }

    pub fn find<'b>(&'b self, symbol: &~str) -> Expr {
        match self.entries.find(symbol) {
            Some(v) => v.clone(),
            None => {
                match &self.outer {
                    &Some(ref outer_env) => outer_env.find(symbol),
                    &None => fail!("No variable named {} defined in the environment."
                                   , *symbol)
                }
            }
        }
    }

    pub fn unwrap_parent(self) -> Env {
        match self.outer {
            Some(p) => *p,
            None => fail!("env.unwrap_parent(): tried to unwrap when there's no parent env")
        }
    }
}

impl Expr {
    pub fn new_atom(input: ~str) -> Expr {
        let first_char = input.char_at(0);
        match first_char {
            '0' | '1' | '2' | '3' | '4' | '5' |
            '6' | '7' | '8' | '9' => {
                match input.contains(".") {
                    true => {
                        let parsed_val: Option<f64>
                            = FromStr::from_str(input);
                        match parsed_val {
                            Some(val) => Atom(Number::float(val)),
                            None => fail!("Cannot parse number-like input of: '{}'", input)
                        }
                    },
                    false => {
                        match FromStr::from_str(input) {
                            Some(val) => Atom(Integer(val)),
                            None => fail!("Cannot parse number-like input of: {}", input)
                        }
                    }
                }
            },
            '#' => {
                // #-prefix parsing
                match &input {
                    v if *v == ~"#f" => Atom(Boolean(false)),
                    v if *v == ~"#false" => Atom(Boolean(false)),
                    v if *v == ~"#t" => Atom(Boolean(true)),
                    v if *v == ~"#true" => Atom(Boolean(true)),
                    _ => fail!("un-implemented case of #-prefixing")
                }
            }
            _ => Atom(Symbol(input))
        }
    }

    pub fn cons(car: Expr, cdr: Expr) -> Expr {
        let mut new_items = vec!(~car);
        match cdr {
            v @ Atom(_) => new_items.push(~v),
            List(cdr_items) => for i in cdr_items.move_iter() {
                new_items.push(i);
            }
        }
        List(new_items)
    }

    pub fn print(&self) -> ~str {
        match self {
            &List(ref items) => {
                let out = items.iter().map(|i| i.print())
                    .fold(~"(", |m, v| m + v + " ");
                out.trim() + ")"
            },
            &Atom(ref v) => v.print()
        }
    }
    pub fn un_cons(self) -> (Expr, Expr) {
        match self {
            Atom(_) => fail!("Cannot un_cons an Atom value"),
            List(mut items) => {
                if items.len() == 0 {
                    fail!("cannot build car/cdr of empty list");
                }
                let car = items.shift()
                    .expect("at least one item in the least; shouldn't happen");
                (*car, List(items))
            }
        }
    }

    // type-evaluating built-ins
    pub fn is_atom(&self) -> bool {
        match *self {
            Atom(_) => true,
            _ => false
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            List(ref items) => items.len() == 0,
            _ => false
        }
    }

    // fns for consuming/breaking down Exprs into composed elements
    // or underlying values
    pub fn unbox_and_eval(self, env: Env) -> (Vec<Expr>, Env) {
        match self {
            List(items) => {
                let args: Vec<Expr> = items.move_iter().map(|x| *x).collect();
                let mut env = env;
                let mut evald_args = Vec::new();
                for i in args.move_iter() {
                    let (evald_arg, out_env) = eval(i, env);
                    let evald_arg =
                        evald_arg
                        .expect("eval'd arg should ret expr");
                    env = out_env;
                    evald_args.push(evald_arg);
                }
                (evald_args, env)
            },
            _ => fail!("calling unbox on non-List expr")
        }
    }
    pub fn unwrap_float(self) -> BigRational {
        match self {
            Atom(Integer(v)) => Ratio::new(v, One::one()),
            Atom(Float(v)) => v,
            _ => fail!("calling unwrap_float() on non-numeric value")
        }
    }
    pub fn unwrap_integer(self) -> BigInt {
        match self {
            Atom(Float(v)) => v.numer().clone(),
            Atom(Integer(v)) => v,
            _ => fail!("calling unwrap_integer() on non-numeric value")
        }
    }
}

impl AtomVal {
    pub fn print(&self) -> ~str {
        match self {
            &Symbol(ref v) => v.to_owned(),
            &Integer(ref v) => v.to_str(),
            &Float(ref v) => Number::float_print(v),
            &Lambda(ref v) => v.print(),
            &Boolean(ref v) => ~"#" + format!("{}", v.to_str().char_at(0))
        }
    }
}

mod Number {
    use super::{AtomVal, Integer, Float};
    use num::bigint::BigInt;
    use num::rational::{Ratio, BigRational};
    use collections::TreeSet;
    use std::char;
    use std::strbuf::StrBuf;
    use std::num::Zero;
    #[allow(dead_code)]
    pub fn integer(val: i64) -> AtomVal {
        let bi: BigInt = FromPrimitive::from_i64(val)
            .expect("Number::integer: should be able to unwrap i64->BigInt");
        Integer(bi)
    }
    pub fn uint(val: uint) -> AtomVal {
        let bi: BigInt = FromPrimitive::from_uint(val)
            .expect("Number::uint: should be able to unwrap uint->BigInt");
        Integer(bi)
    }
    pub fn float(val: f64) -> AtomVal {
        let br = Ratio::from_float(val)
            .expect("Number::float: should be able to unwrap f64->BigInt");
        Float(br)
    }
    // adapted from https://gist.github.com/kballard/4771f0d338fbb1896446
    pub fn float_print(v: &BigRational) -> ~str {
        let mut s = StrBuf::from_owned_str(v.to_integer().to_str());
        let mut v = v.fract();
        if !v.is_zero() {
            s.push_char('.');
            let ten: BigInt = FromPrimitive::from_int(10).unwrap();
            let ten = Ratio::from_integer(ten);
            let mut seen = TreeSet::new();
            while !v.is_zero() {
                v = v * ten;
                if seen.contains(&v) {
                    s.push_str("…");
                    break;
                }
                seen.insert(v.clone());
                let digit = v.to_integer(); // should only be 1 digit
                // bail out when we hit a zero in the decimal portion
                if digit.is_zero() {
                    break;
                }
                s.push_char(char::from_digit(digit.to_uint().unwrap(), 10).unwrap());
                v = v.fract();
            }
        }
        s.into_owned()
    }
}

#[cfg(test)]
mod parser_tests {
    use super::{pad_input, tokenize, parse, Atom, List, Symbol,
                read, Boolean, Number};

    #[test]
    fn pad_input_should_insert_spaces_before_and_after_parens() {
        assert_eq!(pad_input(~"(x 1 2 3)"), ~" ( x 1 2 3 ) ");
    }

    #[test]
    fn tokenize_string_into_tokens() {
        let tokens = tokenize(~"(foo 12 3)");
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens.get(0).to_owned(), ~"(");
        assert_eq!(tokens.get(1).to_owned(), ~"foo");
        assert_eq!(tokens.get(2).to_owned(), ~"12");
        assert_eq!(tokens.get(3).to_owned(), ~"3");
        assert_eq!(tokens.get(4).to_owned(), ~")");
    }

    #[test]
    fn should_parse_tokens_consisting_of_a_str_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize(~"bar");
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(Symbol(val)) => {
                assert_eq!(val, ~"bar")
            },
            _ => assert!(false)
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_an_int_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize(~"42");
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(val) => {
                assert_eq!(val, Number::integer(42))
            },
            _ => assert!(false)
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_a_float_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize(~"1.1");
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(val) => {
                assert_eq!(val, Number::float(1.1))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn should_parse_tokens_consisting_of_a_list_of_atoms_and_parse_it() {
        let tokens = &mut tokenize(~"(bar 12 45)");
        let parsed_item = parse(tokens);
        assert_eq!(parsed_item.is_atom(), false);
        match parsed_item {
            List(items) => {
                let items_len = items.len();
                assert!(3 == items_len);
                assert_eq!(items.get(0).is_atom(), true);
                assert_eq!(*items.get(0), ~Atom(Symbol(~"bar")));
                assert_eq!(items.get(1).is_atom(), true);
                assert_eq!(*items.get(1), ~Atom(Number::integer(12)));
                assert_eq!(items.get(2).is_atom(), true);
                assert_eq!(*items.get(2), ~Atom(Number::integer(45)));
            },
            _ => fail!("got back an atom, it seems")
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_a_list_of_atoms_and_a_nested_list() {
        let tokens = &mut tokenize(~"((2 3) bar 12 (hee (hah)))");
        let parsed_item = parse(tokens);
        assert_eq!(parsed_item.is_atom(), false);
        match parsed_item {
            List(items) => {
                let items_len = items.len();
                assert!(4 == items_len);
                match *items.get(0) {
                    ~List(ref items) => {
                        assert_eq!(items.len(), 2);
                        assert_eq!(*items.get(0), ~Atom(Number::integer(2)));
                        assert_eq!(*items.get(1), ~Atom(Number::integer(3)));
                    },
                    _ => fail!("shoulda got a list")
                }
                assert_eq!(*items.get(1), ~Atom(Symbol(~"bar")));
                assert_eq!(*items.get(2), ~Atom(Number::integer(12)));
                match *items.get(3) {
                    ~List(ref items) => {
                        assert_eq!(items.len(), 2);
                        assert_eq!(*items.get(0), ~Atom(Symbol(~"hee")));
                        match *items.get(1) {
                            ~List(ref items) => {
                                assert_eq!(items.len(), 1);
                                assert_eq!(*items.get(0), ~Atom(Symbol(~"hah")));
                            },
                            _ => fail!("shoulda got a list")
                        }
                    },
                    _ => fail!("shoulda got a list")
                }
            },
            _ => fail!("got back an atom, it seems")
        }
    }
    #[test]
    fn parsed_tokens_of_a_list_of_atoms_and_a_nested_list_should_to_str_correctly() {
        let input = ~"((2 3) bar 12 (hee (hah)))";
        let tokens = &mut tokenize(input.to_owned());
        let parsed_item = parse(tokens);
        let output = parsed_item.print();
        assert_eq!(input, output);
    }
    #[test]
    fn commas_should_be_treated_as_whitespace() {
        let expr = read(~"(x, y,z)");
        match &expr {
            &List(ref items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items.get(0), &~Atom(Symbol(~"x")));
                assert_eq!(items.get(1), &~Atom(Symbol(~"y")));
                assert_eq!(items.get(2), &~Atom(Symbol(~"z")));
            }, _ => fail!("expected a list")
        }
        assert_eq!(expr.print(), ~"(x y z)")
    }
    #[test]
    fn can_parse_boolean_literals() {
        let false_atom = read(~"#f");
        assert_eq!(false_atom, Atom(Boolean(false)));
        let true_atom = read(~"#t");
        assert_eq!(true_atom, Atom(Boolean(true)));
        let false_atom = read(~"#false");
        assert_eq!(false_atom, Atom(Boolean(false)));
        let true_atom = read(~"#true");
        assert_eq!(true_atom, Atom(Boolean(true)));
    }
}

#[cfg(test)]
mod eval_tests {
    use super::{add_builtins, read, Atom, List, Lambda, Symbol,
                Env, eval, UserDefined, Number};
    mod un_cons {
        use super::super::{read, Atom, List, Symbol};
        #[should_fail]
        #[test]
        fn an_atom_expr_returns_the_atom_in_the_car_with_none_in_the_cdr() {
            let expr = read(~"x");
            expr.un_cons();
        }

        #[test]
        fn a_list_with_one_atom_element_returns_an_atom_car_and_an_empty_list_in_the_cdr() {
            let expr = read(~"(x)");
            let (car, cdr) = expr.un_cons();
            assert_eq!(car, Atom(Symbol(~"x")));
            match cdr {
                List(items) => assert_eq!(items.len(), 0),
                _ => fail!("expected a list")
            }
        }

        #[test]
        fn a_list_with_multiple_elems_puts_the_first_the_car_and_the_rest_in_the_cdr() {
            let expr = read(~"(y 2 3)");
            let (car, cdr) = expr.un_cons();
            assert_eq!(car, Atom(Symbol(~"y")));
            match cdr {
                List(items) => {
                    assert_eq!(items.len(), 2)
                },
                _ => fail!("expected a list")
            }
        }

        #[test]
        #[should_fail]
        fn calling_un_cons_on_an_empty_list_should_fail() {
            let expr = read(~"()");
            expr.un_cons();
        }
    }

    #[test]
    fn given_a_symbol_in_the_env_then_calling_eval_should_resolve_its_value() {
        let env = Env::new(
            Some(vec!(~"x")),
            Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read(~"x");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.expect("should return an expr"),
                   Atom(Number::integer(42)));
    }

    #[test]
    #[should_fail]
    fn given_a_symbol_NOT_in_the_env_then_calling_eval_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"x");
        eval(in_expr, env);
    }

    #[test]
    fn given_a_float_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"34.3");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(34.3)));
    }

    #[test]
    fn given_a_integer_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"34");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(34)));
    }

    #[test]
    fn given_a_quote_of_an_atom_expr_should_resolve_to_just_the_atom() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(quote 42)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(42)));
    }

    #[test]
    #[should_fail]
    fn a_quote_with_no_params_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(quote)");
        eval(in_expr, env);
    }

    #[test]
    fn given_a_quote_of_a_list_expr_it_should_resolve_to_the_list_without_resolving_symbols() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(quote (x 37))");
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(*items.get(0), ~Atom(Symbol(~"x")));
                assert_eq!(*items.get(1), ~Atom(Number::integer(37)));
            },
            _ => fail!("expected a list")
        }
    }

    #[test]
    fn only_an_empty_list_expr_should_have_is_null_return_true() {
        let empty_list = read(~"()");
        assert_eq!(empty_list.is_null(), true);
        let not_null = read(~"(())");
        assert_eq!(not_null.is_null(), false);
        let not_null = read(~"2");
        assert_eq!(not_null.is_null(), false);
        let not_null = read(~"x");
        assert_eq!(not_null.is_null(), false);
        let not_null = read(~"34.4");
        assert_eq!(not_null.is_null(), false);
        let not_null = read(~"(x 3 4)");
        assert_eq!(not_null.is_null(), false);
    }

    #[test]
    fn an_if_call_with_a_non_false_test_should_resolve_the_conseq_branch() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) =
            eval(read(~"(if (quote 1)(quote conseq) (quote alt))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"conseq")));
        let (out_expr, env) =
            eval(read(~"(if #t (quote conseq) (quote alt))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"conseq")));
        let (out_expr, _) =
            eval(read(~"(if (list)(quote conseq) (quote alt))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"conseq")));
    }

    #[test]
    fn an_if_call_with_a_false_test_should_resolve_the_alt_branch() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(if #f (quote conseq) (quote alt))");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"alt")));
    }

    #[test]
    fn calling_eval_with_an_empty_list_should_return_the_list_as_a_value() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"()");
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            list @ List(_) => {
                assert_eq!(list.is_null(), true);
            },
            _ => fail!("expect a list")
        }
    }

    #[test]
    #[should_fail]
    fn calling_set_on_a_var_not_in_the_scope_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(set! x 123)");
        eval(in_expr, env);
    }

    #[test]
    fn calling_set_on_a_var_in_the_scope_should_succeed() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read(~"(set! x 123)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(123)));
    }

    #[test]
    fn calling_set_should_resolve_the_val() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read(~"(set! x (quote 37))");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(37)));
    }

    #[test]
    fn calling_set_on_a_var_in_an_outer_scope_should_succeed() {
        let outer_env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Number::integer(42)))),
            None);
        let env = Env::new(
            None, None,
            Some(outer_env));
        let in_expr = read(~"(set! x 43)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(43)));
    }

    #[test]
    fn set_should_return_a_None_for_the_out_expr() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read(~"(set! x 43)");
        let (result, _) = eval(in_expr, env);
        assert_eq!(result, None);
    }

    #[test]
    #[should_fail]
    fn calling_set_with_a_non_symbol_as_the_atom_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(set! 1 123)");
        eval(in_expr, env);
    }

    #[test]
    fn an_if_test_that_returns_a_None_out_expr_should_run_the_conseq_branch() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read(~"(if (set! x 37) (quote conseq) (quote alt))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"conseq")));
        assert_eq!(env.find(&~"x"), Atom(Number::integer(37)));
    }

    #[test]
    fn should_be_able_to_create_an_env_with_all_None_params() {
        Env::new(None, None, None);
    }

    #[test]
    fn evaling_a_define_should_return_None() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define x 123)");
        let (result, _) = eval(in_expr, env);
        assert!(result.is_none());
    }

    #[test]
    fn defining_a_var_should_set_it_in_the_scope() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define x 123)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(123)));
    }

    #[test]
    fn calling_define_should_resolve_the_val_of_the_input() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define x (quote foo))");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Symbol(~"foo")));
    }

    #[test]
    fn defining_a_var_already_set_in_an_outer_scope_should_shadow_it_in_the_inner_scope() {
        let outer_env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Number::integer(42)))),
            None);
        let env = Env::new(None, None, Some(outer_env));
        let in_expr = read(~"(define x 37)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(37)));
        let outer_env = env.unwrap_parent();
        assert_eq!(outer_env.find(&~"x"), Atom(Number::integer(42)));
    }

    #[test]
    fn lambda_exprs_should_expect_a_list_for_vars_and_an_expr_for_body() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(lambda (x) 37)");
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(_, vars, body) => {
                        assert_eq!(vars.len(), 1);
                        assert_eq!(vars.get(0), &~"x");
                        assert_eq!(*body, Atom(Number::integer(37)));
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
    }
    #[test]
    fn lambda_exprs_evald_outside_of_define_have_the_name_of_anonymous() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(lambda (x y) 37)");
        let (out_expr, _) = eval(in_expr, env);
        let out_expr = out_expr.unwrap();
        let out_expr_as_str = out_expr.print();
        match out_expr {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(name, _, _) => {
                        assert_eq!(name, ~"anonymous");
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
        assert_eq!(out_expr_as_str, ~"lambda:anonymous(x y)");
    }
    #[test]
    fn lambda_exprs_evald_within_a_define_have_the_of_the_provided_var_symbol() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define foo (lambda (x y) 37))");
        let (_, env) = eval(in_expr, env);
        let out_expr = env.find(&~"foo");
        let out_expr_as_str = out_expr.print();
        match out_expr {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(name, _, _) => {
                        assert_eq!(name, ~"foo");
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
        assert_eq!(out_expr_as_str, ~"lambda:foo(x y)");
    }
    #[test]
    #[should_fail]
    fn lambda_expr_eval_should_fail_if_a_list_isnt_in_the_vars_position() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define foo (lambda x 37))");
        eval(in_expr, env);
    }
    #[test]
    #[should_fail]
    fn lambda_expr_eval_should_fail_if_theres_no_third_position() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define foo (lambda (x)))");
        eval(in_expr, env);
    }

    #[test]
    fn begin_should_return_the_evald_atom_expr_of_the_tail_param() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(begin (define x 37) 42)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(37)));
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(42)));
    }
    #[test]
    fn begin_should_return_no_expr_if_its_tail_param_returns_nothing() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(begin (define x 37))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(37)));
        assert_eq!(out_expr, None);
    }
    #[test]
    fn begin_can_have_a_list_in_the_tail_position() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(begin (define x 37) (quote (1 x 3.4)))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Number::integer(37)));
        match out_expr.unwrap() {
            List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(*items.get(0), ~Atom(Number::integer(1)));
                assert_eq!(*items.get(1), ~Atom(Symbol(~"x")));
                assert_eq!(*items.get(2), ~Atom(Number::float(3.4)));
            }, _ => fail!("expected a list")
        }
    }
    #[test]
    fn can_define_a_var_with_the_same_name_as_a_special_form_keyword_but_SF_is_still_usable() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(begin (define set! 42) (set! set! 37) set!)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(37)));
    }

    #[test]
    fn can_define_a_lambda_that_returns_an_atom_and_subsequently_invoke_it() {
        let env = Env::new(None, None, None);
        let in_expr = read(~"(define get-1 (lambda () 1))");
        let (_, env) = eval(in_expr, env);
        let in_expr = read(~"(get-1)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(1)));
    }

    #[test]
    fn can_define_a_lambda_that_takes_an_arg_and_invoke_it() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(
            ~"(begin (define double (lambda (x) (+ x x))) (double 3))");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(6)));
    }

    #[test]
    fn builtin_print_sanity_check() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"+");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap().print(), ~"builtin-fn:+");
    }
}

#[cfg(test)]
mod builtins_tests {
    use super::{read, add_builtins, eval, Env, Atom,
                Boolean, Number};
    #[test]
    fn two_plus_two_equals_four() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(+ 2 2)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(4)));
    }
    #[test]
    fn can_sum_an_arbitrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(+ 2 2 1 1 1 1 4)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(12)));
    }
    #[test]
    fn two_point_five_plus_two_equals_four_point_five() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(+ 2.5 2)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(4.5)));
    }
    #[test]
    fn four_minus_three_equals_one() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(- 4 3)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(1)));
    }
    #[test]
    fn four_point_five_minus_one_equals_equals_three_point_five() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(- 4.5 1)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(3.5)));
    }
    #[test]
    fn can_subtract_an_arbitrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(- 4.5 1 3)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(0.5)));
    }
    #[test]
    fn two_times_twenty_one_equals_42() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(* 2 21)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(42)));
    }
    #[test]
    fn can_multiply_an_aribtrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(* 4 5 3)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(60)));
    }
    #[test]
    fn two_times_one_point_five_equals_three() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(* 2 1.5)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(3.0)));
    }
    #[test]
    fn ten_divided_by_5_equals_2() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(/ 10 5)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(2)));
    }
    #[test]
    fn five_divided_by_two_point_five_equals_2() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(/ 5 2.5)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(2.0)));
    }
    #[test]
    fn can_divide_an_arbitrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(/ 100 10 5)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(2)));
    }
    #[test]
    fn lt_works() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(< 2 4)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(< 5 1)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read(~"(< 1.1 1.2)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(< 1.1 2)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(< 1.0 1)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read(~"(< 1 1)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn gt_works() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(~"(> 4 2)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(> 1 5)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read(~"(> 1.2 1.1)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(> 2 1.1)");
        let (out_expr,env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(not #f)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(not (quote #f))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read(~"(> 1.0 1)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read(~"(> 1 1)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn a_call_to_not_with_anything_that_isnt_false_returns_false() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) = eval(read(~"(not #t)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(not (quote ()))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(not 1)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(not 1.3)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(not (quote 1))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(not (lambda (x) x))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, _) = eval(read(~"(not (+ 1 1))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn gte_works() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) = eval(read(~"(>= 2 2)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read(~"(>= 2 2.0)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read(~"(>= 2 1.9)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read(~"(>= 1.9 2)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, _) = eval(read(~"(>= 1 2)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn lte_works() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) = eval(read(~"(<= 2 2)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read(~"(<= 2 2.0)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read(~"(<= 2 1.9)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(<= 1.9 2)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, _) = eval(read(~"(<= 1 2)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
    }
    #[test]
    #[should_fail]
    fn lt_fails_with_less_than_two_args() {
        eval(read(~"(< 2)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn gt_fails_with_less_than_two_args() {
        eval(read(~"(> 2)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn lte_fails_with_less_than_two_args() {
        eval(read(~"(<= 2)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn gte_fails_with_less_than_two_args() {
        eval(read(~"(>= 2)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn equal_works() {
        let (out_expr, _) = eval(read(~"(= 1 1)"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(true))));
    }
    #[test]
    #[should_fail]
    fn equal_fails_with_less_than_two_params() {
        eval(read(~"(= 1)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn two_identical_lambdas_are_eq() {
        let (out_expr, _) = eval(read(~"(= (lambda (x) x) (lambda (x) x))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(true))));
    }
    #[test]
    fn two_lambdas_with_different_arg_lists_and_matching_bodies_arent_equal() {
        let (out_expr, _) = eval(read(~"(= (lambda (y) 1) (lambda (x) 1))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(false))));
    }
    #[test]
    fn two_lambdas_with_matching_args_lists_and_different_bodies_arent_equal() {
        let (out_expr, _) = eval(read(~"(= (lambda (y) 1) (lambda (y) 2))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(false))));
    }
    #[test]
    fn getting_the_length_of_a_three_element_list_should_return_three() {
        let (out_expr, _) = eval(read(~"(length (quote (1 2 3)))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Number::integer(3))));
    }
    #[test]
    fn getting_the_length_of_an_empty_list_should_return_zero() {
        let (out_expr, _) = eval(read(~"(length (quote ()))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Number::integer(0))));
    }
    #[test]
    #[should_fail]
    fn getting_the_length_of_an_atom_value_should_fail() {
        eval(read(~"(length 1)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn cons_should_combine_two_atoms_into_a_new_list() {
        let (out_expr, _) = eval(read(~"(cons 1 2)"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"(1 2)");
    }
    #[test]
    fn cons_should_combine_an_atom_and_a_list_into_a_new_list() {
        let (out_expr, _) = eval(read(~"(cons 1 (quote (2 3)))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"(1 2 3)");
    }
    #[test]
    fn cons_should_combone_a_list_car_and_a_nested_list_cdr_correctly() {
        let (out_expr, _) = eval(read(~"(cons (quote (1 2 3)) (quote ((4 5 6))))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"((1 2 3) (4 5 6))");
    }
    #[test]
    #[should_fail]
    fn cons_should_fail_with_one_arg() {
        eval(read(~"(cons 1)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn cons_should_fail_with_more_than_two_args() {
        eval(read(~"(cons 1 2 3)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn car_should_pull_the_first_element_from_a_list() {
        let (out_expr, _) = eval(read(~"(car (quote (1 2 3)))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"1");
    }
    #[test]
    #[should_fail]
    fn car_should_fail_when_applied_to_an_atom_value() {
        eval(read(~"(car 1)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn cdr_should_pull_the_remaining_elements_from_a_list() {
        let (out_expr, _) = eval(read(~"(cdr (quote (1 2 3)))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"(2 3)");
    }
    #[test]
    fn cdr_should_return_an_empty_list_when_applied_to_a_single_element_list() {
        let (out_expr, _) = eval(read(~"(cdr (quote (1)))"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"()");
    }
    #[test]
    #[should_fail]
    fn cdr_should_fail_when_applied_to_an_atom_value() {
        eval(read(~"(cdr 1)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn append_adds_an_expr_to_the_end_of_a_list() {
        let (out_expr, _) = eval(read(~"(append (quote (1)) 2)"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"(1 2)");
    }
    #[test]
    #[should_fail]
    fn append_should_fail_if_the_first_arg_isnt_a_list() {
        eval(read(~"(append 1 2"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn append_should_fail_when_passed_one_arg() {
        eval(read(~"(append (quote (1)))"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn append_should_fail_with_more_than_two_args() {
        eval(read(~"(append (quote (1)) 1 2)"),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn applying_list_with_zero_args_should_return_an_empty_list() {
        let (out_expr, _) = eval(read(~"(list)"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().is_null(), true);
    }
    #[test]
    fn applying_list_to_any_number_of_arguments_returns_a_list_of_those_arguments() {
        let (out_expr, env) = eval(read(~"(list 1 2 3)"),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), ~"(1 2 3)");
        let (out_expr, _) = eval(read(~"(list 1 (list 2) 3)"), env);
        assert_eq!(out_expr.unwrap().print(), ~"(1 (2) 3)");
    }
    #[test]
    fn list_predicate_returns_false_for_atom_values() {
        let (out_expr, _) = eval(read(~"(list? 1)"),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn list_predicate_returns_true_for_any_list_value() {
        let (out_expr, env) = eval(read(~"(list? (list))"),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, _) = eval(read(~"(list? (list 1 2 3))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
    }
    #[test]
    fn null_predicate_returns_true_only_for_empty_list_values() {
        let (out_expr, env) = eval(read(~"(null? (list))"),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read(~"(null? (quote x))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, _) = eval(read(~"(null? (list 1 2 3))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn symbol_predicate_returns_true_only_for_empty_list_values() {
        let (out_expr, env) = eval(read(~"(symbol? (list))"),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read(~"(symbol? (quote x))"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, _) = eval(read(~"(symbol? 1)"), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }

    #[test]
    fn norvig_suite_1() {
        let one = ~"(define area (lambda (r) (* 3.141592653 (* r r))))";
        let (_, env) = eval(read(one),
                            add_builtins(Env::new(None, None, None)));
        let two = ~"(area 3)";
        let (out_expr, env) = eval(read(two), env);
        //assert_eq!(out_expr.unwrap().print().contains("28.27433"), true);
        assert_eq!(out_expr.unwrap().print(), ~"28.274333877");
        let three = ~"\
            (define fact2                         \
                (lambda (n acc)                   \
                    (if (<= n 1)                  \
                        acc                       \
                        (fact2 (- n 1) (* n acc)) \
                    )                             \
                )                                 \
            )";
        let (_, env) = eval(read(three), env);
        let three_point_five = ~"(define fact (lambda (n) (fact2 n 1)))";
        let (_, env) = eval(read(three_point_five), env);
        let four = ~"(fact 10)";
        let (out_expr, env) = eval(read(four), env);
        assert_eq!(out_expr.unwrap().print(), ~"3628800");
        let five = ~"(fact 100)";
        let (out_expr, _) = eval(read(five), env);
        assert_eq!(out_expr.unwrap().print(), ~"9332621544394415268169923885626"+
                   "67004907159682643816214685929638952175999932299156089414639"+
                   "76156518286253697920827223758251185210916864000000000000000"+
                   "000000000");
    }
}
