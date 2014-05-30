// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, Atom, List, Symbol, Boolean, Lambda, UserDefined, BuiltIn};
use env::Env;

pub fn eval<'env>(expr: Expr, env: Env) -> (Option<Expr>, Env) {
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
                Atom(ref val) if *val == Symbol("quote".to_owned())  => {
                    match cdr {
                        List(mut items) =>
                            (Some(*items.shift().expect("eval: quote list shouldnt be empty")),
                             env),
                        _ => fail!("eval: quote: expected List in cdr position")
                    }
                },
                Atom(ref val) if *val == Symbol("if".to_owned()) => {
                    eval_if(cdr, env)
                },
                Atom(ref val) if *val == Symbol("set!".to_owned()) => {
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
                Atom(ref val) if *val == Symbol("define".to_owned()) => {
                    (None, eval_define(cdr, env))
                },
                Atom(ref val) if *val == Symbol("lambda".to_owned()) => {
                    (eval_lambda("anonymous".to_owned(), cdr), env)
                },
                Atom(ref val) if *val == Symbol("begin".to_owned()) => {
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
                                    Atom(ref val) if *val == Symbol("lambda".to_owned()) => {
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
                    for v in vars.move_iter() {
                        match v {
                            box Atom(Symbol(var_name)) => {
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
