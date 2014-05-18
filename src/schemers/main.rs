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
use collections::hashmap::HashMap;

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
    let parsed_items = parse_str(program);
    println!("{}", parsed_items.print());
}

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
    Float(f64)
}

fn eval(expr: Expr, env: Env) -> (Expr, Env) {
    match expr {
        // Atom values and values in the Env
        Atom(Symbol(ref var_name)) => (env.find(var_name), env),
        val @ Atom(_) => (val, env),
        list @ List(_) => {
            let (car, cdr) = list.un_cons();
            match car {
                Atom(ref val) if *val == Symbol(~"quote")  => {
                    match cdr {
                        List(mut items) => {
                            (*items.shift().expect("eval: quote list shouldnt be empty"), env)
                        },
                        _ => fail!("eval: expected List in cdr position")
                    }
                },
                _ => fail!("un-implemented case of eval")
            }
        }
    }
}

fn parse_str(input: ~str) -> Expr {
    parse(&mut tokenize(input))
}

fn pad_input(input: ~str) -> ~str {
    input.replace("(", " ( ").replace(")", " ) ")
}

fn tokenize(input: ~str) -> Vec<~str> {
    pad_input(input).split_str(" ")
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
    pub fn new(params: Option<Vec<~str>>, args: Option<Vec<Expr>>, outer: Option<~Env>) -> Env {
        let mut entries = HashMap::new();
        if params.is_some() && args.is_some() {
            let params = params.expect("params should be a value");
            let args = args.expect("args should be a value");
            if params.len() == args.len() {
                for ctr in range(0,params.len()) {
                    entries.insert(params.get(ctr).to_owned(),
                                   args.get(ctr).clone());
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
        Env { entries: entries, outer: outer }
    }

    pub fn find(&self, symbol: &~str) -> Expr {
        match self.entries.find(symbol) {
            Some(expr) => expr.clone(),
            None => {
                match &self.outer {
                    &Some(ref outer_env) => outer_env.find(symbol),
                    &None => fail!("No variable named {} defined in the environment."
                                   , *symbol)
                }
            }
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
            _ => Atom(Symbol(input))
        }
    }

    pub fn is_atom(&self) -> bool {
        match *self {
            Atom(_) => true,
            _ => false
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
}

impl AtomVal {
    pub fn print(&self) -> ~str {
        match self {
            &Symbol(ref v) => v.to_owned(),
            &Integer(ref v) => v.to_str(),
            &Float(ref v) => v.to_str()
        }
    }
}

#[cfg(test)]
mod parser_test {
    use super::{pad_input, tokenize, parse, Atom, List, Symbol, Integer, Float};

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
        println!("{:?}", parsed_item);
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
        println!("{:?}", parsed_item);
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
        println!("{:?}", parsed_item);
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
}

#[cfg(test)]
mod eval_test {
    use super::{parse_str, Atom, List, Symbol, Integer, Float,
                       Env, eval};
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
        assert_eq!(out_expr, Atom(Integer(42)));
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
        assert_eq!(out_expr, Atom(Float(34.3)));
    }

    #[test]
    fn given_a_integer_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"34");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr, Atom(Integer(34)));
    }

    #[test]
    fn given_a_quote_of_an_atom_expr_should_resolve_to_just_the_atom() {
        let env = Env::new(None, None, None);
        let in_expr = parse_str(~"(quote 42)");
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr, Atom(Integer(42)));
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
        match out_expr {
            List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(*items.get(0), ~Atom(Symbol(~"x")));
                assert_eq!(*items.get(1), ~Atom(Integer(37)));
            },
            _ => fail!("expected a list")
        }
    }

    mod env_tests {
        use super::super::{Env};

        #[test]
        fn a_new_env_with_no_parent_params_or_vars_should_be_set_up_empty() {
            let _env = Env::new(None, None, None);
        }
    }
}
