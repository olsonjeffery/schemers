// Copyright 2013-2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
#![crate_id="http://olsonjeffery.github.io#schemers:0.0.1"]
#![desc = "Simple Scheme Interpreter"]
#![license = "3-Clause BSD"]

use std::from_str::FromStr;

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
    let tokens = &mut tokenize(program);
    let parsed_items = parse(tokens);
    println!("{}", parsed_items.print());
}

// parser impl
fn pad_input(input: ~str) -> ~str {
    input.replace("(", " ( ").replace(")", " ) ")
}

fn tokenize(input: ~str) -> Vec<~str> {
    pad_input(input).split_str(" ")
        .filter(|i| *i != "").map(|i| i.to_owned()).collect()
}

#[deriving(Eq, Show)]
pub enum Expression {
    Atom(AtomType),
    List(Vec<~Expression>)
}
impl Expression {
    pub fn new_atom(input: ~str) -> Expression {
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
}

#[deriving(Eq, Show)]
pub enum AtomType {
    Symbol(~str),
    Integer(i64),
    Float(f64)
}

impl AtomType {
    pub fn print(&self) -> ~str {
        match self {
            &Symbol(ref v) => v.to_owned(),
            &Integer(ref v) => v.to_str(),
            &Float(ref v) => v.to_str()
        }
    }
}

fn parse(tokens: &mut Vec<~str>) -> Expression {
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
        x => Expression::new_atom(x)
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
