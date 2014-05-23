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

use std::from_str::FromStr;
use std::fmt::{Show, Formatter, Result};
use collections::hashmap::HashMap;
use std::num::{from_f64, from_i64};

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
    match eval(parse_str(program), env) {
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
    Integer(i64),
    Float(f64),
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
            &UserDefined(ref name, _, ref body) => match other {
                &UserDefined(ref other_name, _, ref other_body) =>
                    name == other_name && body == other_body,
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
                    match cdr {
                        List(mut items) => {
                            if items.len() != 3 {
                                fail!("eval: if: expect three entries in if cdr list");
                            }
                            let (out_expr, out_env) = eval(
                                *items.shift().unwrap(), env);
                            match out_expr {
                                Some(test_result) => {
                                    if test_result.is_null() == false {
                                        eval(*items.shift().unwrap(), out_env)
                                    } else {
                                        eval(*items.pop().unwrap(), out_env)
                                    }
                                }, None => eval(*items.shift().unwrap(), out_env)
                            }
                        },
                        _ => fail!("eval: if: expected List in cdr position")
                    }
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
                                          "eval: set!: provided val didn't resolve to anything"));
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

// standard procedures impl
fn add_globals(mut env: Env) -> Env {
    env.define(~"+", Atom(Lambda(BuiltIn(~"+", builtin_add))));
    env.define(~"-", Atom(Lambda(BuiltIn(~"-", builtin_subtract))));
    env
}
fn builtin_add(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env) {
    // run through all the args to see if there're any
    // floats.. we'll return a float if so.. otherwise
    // we return an int
    let mut hasFloats = false;
    for atom in args.iter() {
        match atom {
            &Atom(Float(_)) => hasFloats = true,
            &Atom(Integer(_)) => {},
            val => fail!("add: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut out_val = 0.0;
        for atom in args.move_iter() {
            match atom {
                Atom(Float(val)) => out_val += val,
                v @ Atom(Integer(_)) =>
                    out_val += v.unwrap_float(),
                _ => fail!("add/sum: invoking with non numeric input")
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = 0;
        for atom in args.move_iter() {
            match atom {
                Atom(Integer(val)) => out_val += val,
                _ => fail!("add/sum: invoking with non numeric input")
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
            val => fail!("add: invoking with non numeric input: '{}'", val.print())
        }
    }
    let out_val = if hasFloats {
        let mut out_val = match args.shift()
            .expect("head of subtract args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_float(),
                _ => fail!("subtract: cannot process non-numeric input")
        };
        for atom in args.move_iter() {
            println!("non-first arg: {} out_val: {}", atom.print(), out_val);
            match atom {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) =>
                    out_val -= v.unwrap_float(),
                _ => fail!("add/sum: invoking with non numeric input")
            }
        }
        Atom(Float(out_val))
    } else {
        let mut out_val = match args.shift()
            .expect("head of subtract args should be Some()") {
                v @ Atom(Integer(_)) | v @ Atom(Float(_)) => v.unwrap_integer(),
                _ => fail!("subtract: cannot process non-numeric input")
        };
        for atom in args.iter() {
            println!("non-first arg: {} out_val: {}", atom.print(), out_val);
            match atom {
                &Atom(Integer(val)) => out_val -= val,
                _ => fail!("add/sum: invoking with non numeric input")
            }
        }
        Atom(Integer(out_val))
    };
    (Some(out_val), env)
}

// parser impl
fn parse_str(input: ~str) -> Expr {
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
                    println!("env::new {}={}", var_name, arg.print());
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
                        match FromStr::from_str(input) {
                            Some(val) => Atom(Float(val)),
                            None => fail!("Cannot parse number-like input of: {}", input)
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
                    println!("lambda arg: {}",evald_arg.print());
                    env = out_env;
                    evald_args.push(evald_arg);
                }
                (evald_args, env)
            },
            _ => fail!("calling unbox on non-List expr")
        }
    }
    pub fn unwrap_float(self) -> f64 {
        match self {
            Atom(Integer(v)) => from_i64(v).unwrap(),
            Atom(Float(v)) => v,
            _ => fail!("calling unwrap_float() on non-numeric value")
        }
    }
    pub fn unwrap_integer(self) -> i64 {
        match self {
            Atom(Float(v)) => from_f64(v).unwrap(),
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
            &Float(ref v) => v.to_str(),
            &Lambda(ref v) => v.print(),
            &Boolean(ref v) => ~"#" + format!("{}", v.to_str().char_at(0))
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::{pad_input, tokenize, parse, Atom, List, Symbol, Integer, Float, parse_str, Boolean};

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
            Atom(Integer(val)) => {
                assert_eq!(val, 42)
            },
            _ => assert!(false)
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_a_float_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize(~"1.1");
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(Float(val)) => {
                assert_eq!(val, 1.1)
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
                assert_eq!(*items.get(1), ~Atom(Integer(12)));
                assert_eq!(items.get(2).is_atom(), true);
                assert_eq!(*items.get(2), ~Atom(Integer(45)));
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
                        assert_eq!(*items.get(0), ~Atom(Integer(2)));
                        assert_eq!(*items.get(1), ~Atom(Integer(3)));
                    },
                    _ => fail!("shoulda got a list")
                }
                assert_eq!(*items.get(1), ~Atom(Symbol(~"bar")));
                assert_eq!(*items.get(2), ~Atom(Integer(12)));
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
        let expr = parse_str(~"(x, y,z)");
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
        let false_atom = parse_str(~"#f");
        assert_eq!(false_atom, Atom(Boolean(false)));
        let true_atom = parse_str(~"#t");
        assert_eq!(true_atom, Atom(Boolean(true)));
        let false_atom = parse_str(~"#false");
        assert_eq!(false_atom, Atom(Boolean(false)));
        let true_atom = parse_str(~"#true");
        assert_eq!(true_atom, Atom(Boolean(true)));
    }
}

#[cfg(test)]
mod eval_tests {
    use super::{add_builtins, parse_str, Atom, List, Lambda, Symbol, Integer, Float,
                       Env, eval, UserDefined};
    mod un_cons {
        use super::super::{parse_str, Atom, List, Symbol};
        #[should_fail]
        #[test]
        fn an_atom_expr_returns_the_atom_in_the_car_with_none_in_the_cdr() {
            let expr = parse_str(~"x");
            expr.un_cons();
        }

        #[test]
        fn a_list_with_one_atom_element_returns_an_atom_car_and_an_empty_list_in_the_cdr() {
            let expr = parse_str(~"(x)");
            let (car, cdr) = expr.un_cons();
            assert_eq!(car, Atom(Symbol(~"x")));
            match cdr {
                List(items) => assert_eq!(items.len(), 0),
                _ => fail!("expected a list")
            }
        }

        #[test]
        fn a_list_with_multiple_elems_puts_the_first_the_car_and_the_rest_in_the_cdr() {
            let expr = parse_str(~"(y 2 3)");
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
            let expr = parse_str(~"()");
            expr.un_cons();
        }
    }

    #[test]
    fn given_a_symbol_in_the_env_then_calling_eval_should_resolve_its_value() {
        let env = Env::new(
            Some(vec!(~"x")),
            Some(vec!(Atom(Integer(42)))),
            None);
        let in_expr = parse_str(~"x");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.expect("should return an expr"), Atom(Integer(42)));
    }

    #[test]
    #[should_fail]
    fn given_a_symbol_NOT_in_the_env_then_calling_eval_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"x");
        eval(in_expr, env);
    }

    #[test]
    fn given_a_float_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"34.3");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Float(34.3)));
    }

    #[test]
    fn given_a_integer_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"34");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(34)));
    }

    #[test]
    fn given_a_quote_of_an_atom_expr_should_resolve_to_just_the_atom() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(quote 42)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(42)));
    }

    #[test]
    #[should_fail]
    fn a_quote_with_no_params_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(quote)");
        eval(in_expr, env);
    }

    #[test]
    fn given_a_quote_of_a_list_expr_it_should_resolve_to_the_list_without_resolving_symbols() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(quote (x 37))");
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(*items.get(0), ~Atom(Symbol(~"x")));
                assert_eq!(*items.get(1), ~Atom(Integer(37)));
            },
            _ => fail!("expected a list")
        }
    }

    #[test]
    fn only_an_empty_list_expr_should_have_is_null_return_true() {
        let empty_list = parse_str(~"()");
        assert_eq!(empty_list.is_null(), true);
        let not_null = parse_str(~"(())");
        assert_eq!(not_null.is_null(), false);
        let not_null = parse_str(~"2");
        assert_eq!(not_null.is_null(), false);
        let not_null = parse_str(~"x");
        assert_eq!(not_null.is_null(), false);
        let not_null = parse_str(~"34.4");
        assert_eq!(not_null.is_null(), false);
        let not_null = parse_str(~"(x 3 4)");
        assert_eq!(not_null.is_null(), false);
    }

    #[test]
    fn an_if_call_with_a_non_null_test_should_resolve_the_conseq_branch() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(if (quote 1) (quote conseq) (quote alt))");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"conseq")));
    }

    #[test]
    fn an_if_call_with_a_null_test_should_resolve_the_alt_branch() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(if (quote ()) (quote conseq) (quote alt))");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"alt")));
    }

    #[test]
    fn calling_eval_with_an_empty_list_should_return_the_list_as_a_value() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"()");
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
        let in_expr = parse_str(~"(set! x 123)");
        eval(in_expr, env);
    }

    #[test]
    fn calling_set_on_a_var_in_the_scope_should_succeed() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Integer(42)))),
            None);
        let in_expr = parse_str(~"(set! x 123)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(123)));
    }

    #[test]
    fn calling_set_should_resolve_the_val() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Integer(42)))),
            None);
        let in_expr = parse_str(~"(set! x (quote 37))");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(37)));
    }

    #[test]
    fn calling_set_on_a_var_in_an_outer_scope_should_succeed() {
        let outer_env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Integer(42)))),
            None);
        let env = Env::new(
            None, None,
            Some(outer_env));
        let in_expr = parse_str(~"(set! x 43)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(43)));
    }

    #[test]
    fn set_should_return_a_None_for_the_out_expr() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Integer(42)))),
            None);
        let in_expr = parse_str(~"(set! x 43)");
        let (result, _) = eval(in_expr, env);
        assert_eq!(result, None);
    }

    #[test]
    #[should_fail]
    fn calling_set_with_a_non_symbol_as_the_atom_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(set! 1 123)");
        eval(in_expr, env);
    }

    #[test]
    fn an_if_test_that_returns_a_None_out_expr_should_run_the_conseq_branch() {
        let env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Integer(42)))),
            None);
        let in_expr = parse_str(~"(if (set! x 37) (quote conseq) (quote alt))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol(~"conseq")));
        assert_eq!(env.find(&~"x"), Atom(Integer(37)));
    }

    #[test]
    fn should_be_able_to_create_an_env_with_all_None_params() {
        Env::new(None, None, None);
    }

    #[test]
    fn evaling_a_define_should_return_None() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(define x 123)");
        let (result, _) = eval(in_expr, env);
        assert!(result.is_none());
    }

    #[test]
    fn defining_a_var_should_set_it_in_the_scope() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(define x 123)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(123)));
    }

    #[test]
    fn calling_define_should_resolve_the_val_of_the_input() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(define x (quote foo))");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Symbol(~"foo")));
    }

    #[test]
    fn defining_a_var_already_set_in_an_outer_scope_should_shadow_it_in_the_inner_scope() {
        let outer_env = Env::new(
            Some(vec!(~"x")), Some(vec!(Atom(Integer(42)))),
            None);
        let env = Env::new(None, None, Some(outer_env));
        let in_expr = parse_str(~"(define x 37)");
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(37)));
        let outer_env = env.unwrap_parent();
        assert_eq!(outer_env.find(&~"x"), Atom(Integer(42)));
    }

    #[test]
    fn lambda_exprs_should_expect_a_list_for_vars_and_an_expr_for_body() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(lambda (x) 37)");
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(_, vars, body) => {
                        assert_eq!(vars.len(), 1);
                        assert_eq!(vars.get(0), &~"x");
                        assert_eq!(*body, Atom(Integer(37)));
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
    }
    #[test]
    fn lambda_exprs_evald_outside_of_define_have_the_name_of_anonymous() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(lambda (x y) 37)");
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
        let in_expr = parse_str(~"(define foo (lambda (x y) 37))");
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
        let in_expr = parse_str(~"(define foo (lambda x 37))");
        eval(in_expr, env);
    }
    #[test]
    #[should_fail]
    fn lambda_expr_eval_should_fail_if_theres_no_third_position() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(define foo (lambda (x)))");
        eval(in_expr, env);
    }

    #[test]
    fn begin_should_return_the_evald_atom_expr_of_the_tail_param() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(begin (define x 37) 42)");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(37)));
        assert_eq!(out_expr.unwrap(), Atom(Integer(42)));
    }
    #[test]
    fn begin_should_return_no_expr_if_its_tail_param_returns_nothing() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(begin (define x 37))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(37)));
        assert_eq!(out_expr, None);
    }
    #[test]
    fn begin_can_have_a_list_in_the_tail_position() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(begin (define x 37) (quote (1 x 3.4)))");
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&~"x"), Atom(Integer(37)));
        match out_expr.unwrap() {
            List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(*items.get(0), ~Atom(Integer(1)));
                assert_eq!(*items.get(1), ~Atom(Symbol(~"x")));
                assert_eq!(*items.get(2), ~Atom(Float(3.4)));
            }, _ => fail!("expected a list")
        }
    }
    #[test]
    fn can_define_a_var_with_the_same_name_as_a_special_form_keyword_but_SF_is_still_usable() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(begin (define set! 42) (set! set! 37) set!)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(37)));
    }

    #[test]
    fn can_define_a_lambda_that_returns_an_atom_and_subsequently_invoke_it() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(define get-1 (lambda () 1))");
        let (_, env) = eval(in_expr, env);
        let in_expr = parse_str(~"(get-1)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(1)));
    }

    #[test]
    fn can_define_a_lambda_that_takes_an_arg_and_invoke_it() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(
            ~"(begin (define double (lambda (x) (+ x x))) (double 3))");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(6)));
    }

    #[test]
    fn builtin_print_sanity_check() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"+");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap().print(), ~"builtin-fn:+");
    }
}

#[cfg(test)]
mod builtin_procedures_test {
    use super::{parse_str, add_globals, eval, Env, Atom, Integer, Float};
    #[test]
    fn two_plus_two_equals_four() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"(+ 2 2)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(4)));
    }
    #[test]
    fn can_sum_an_arbitrary_number_of_items() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"(+ 2 2 1 1 1 1 4)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(12)));
    }
    #[test]
    fn two_point_five_plus_two_equals_four_point_five() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"(+ 2.5 2)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Float(4.5)));
    }
    #[test]
    fn four_minus_three_equals_one() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"(- 4 3)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Integer(1)));
    }
    #[test]
    fn four_point_five_minus_one_equals_equals_three_point_five() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"(- 4.5 1)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Float(3.5)));
    }
    #[test]
    fn can_subtract_an_arbitrary_number_of_items() {
        let env = add_globals(Env::new(None, None, None));
        let in_expr = parse_str(~"(- 4.5 1 3)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Float(0.5)));
    }
}
