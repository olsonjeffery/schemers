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
    println!("{:?}", read(tokens));
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
pub enum ParseItem {
    AtomItem(Atom),
    ListItem(Vec<~ParseItem>)
}
impl ParseItem {
    pub fn new_atom(input: ~str) -> ParseItem {
        let first_char = input.char_at(0);
        match first_char {
            '0' | '1' | '2' | '3' | '4' | '5' |
            '6' | '7' | '8' | '9' => {
                let val = FromStr::from_str(input);
                match val {
                    Some(val) => AtomItem(IntAtom(val)),
                    None => fail!("Cannot parse number-like input of: {}", input)
                }
            },
            _ => AtomItem(StrAtom(input))
        }
    }
}

#[deriving(Eq, Show)]
pub enum Atom {
    StrAtom(~str),
    IntAtom(int)
}

impl ParseItem {
    pub fn is_atom(&self) -> bool {
        match *self {
            AtomItem(_) => true,
            _ => false
        }
    }
}

fn read(tokens: &mut Vec<~str>) -> ParseItem {
    let current_token =
        tokens.shift().expect("calling read() w/ empty token list; shouldn't happen");
    match current_token {
        ref x if *x == ~"(" => {
            let mut list = Vec::new();
            while *tokens.get(0) != ~")" {
                list.push(box read(tokens));
            }
            tokens.shift();
            ListItem(list)
        },
        ref x if *x == ~")" => fail!("hit ) token; shouldn't happen"),
        x => ParseItem::new_atom(x)
    }
}

#[cfg(test)]
mod parser_test {
    use super::{pad_input, tokenize, read, AtomItem, ListItem, StrAtom, IntAtom};

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
        let parsed_item = read(tokens);
        println!("{:?}", parsed_item);
        match parsed_item {
            AtomItem(StrAtom(val)) => {
                assert_eq!(val, ~"bar")
            },
            _ => assert!(false)
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_an_int_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize(~"42");
        let parsed_item = read(tokens);
        println!("{:?}", parsed_item);
        match parsed_item {
            AtomItem(IntAtom(val)) => {
                assert_eq!(val, 42)
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn should_parse_tokens_consisting_of_a_list_of_atoms_and_parse_it() {
        let tokens = &mut tokenize(~"(bar 12 45)");
        let parsed_item = read(tokens);
        assert_eq!(parsed_item.is_atom(), false);
        match parsed_item {
            ListItem(items) => {
                let items_len = items.len();
                assert!(3 == items_len);
                assert_eq!(items.get(0).is_atom(), true);
                assert_eq!(*items.get(0), ~AtomItem(StrAtom(~"bar")));
                assert_eq!(items.get(1).is_atom(), true);
                assert_eq!(*items.get(1), ~AtomItem(IntAtom(12)));
                assert_eq!(items.get(2).is_atom(), true);
                assert_eq!(*items.get(2), ~AtomItem(IntAtom(45)));
            },
            _ => fail!("got back an atom, it seems")
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_a_list_of_atoms_and_a_nested_list() {
        let tokens = &mut tokenize(~"((2 3) bar 12 (hee (hah)))");
        let parsed_item = read(tokens);
        assert_eq!(parsed_item.is_atom(), false);
        match parsed_item {
            ListItem(items) => {
                let items_len = items.len();
                assert!(4 == items_len);
                match *items.get(0) {
                    ~ListItem(ref items) => {
                        assert_eq!(items.len(), 2);
                        assert_eq!(*items.get(0), ~AtomItem(IntAtom(2)));
                        assert_eq!(*items.get(1), ~AtomItem(IntAtom(3)));
                    },
                    _ => fail!("shoulda got a list")
                }
                assert_eq!(*items.get(1), ~AtomItem(StrAtom(~"bar")));
                assert_eq!(*items.get(2), ~AtomItem(IntAtom(12)));
                match *items.get(3) {
                    ~ListItem(ref items) => {
                        assert_eq!(items.len(), 2);
                        assert_eq!(*items.get(0), ~AtomItem(StrAtom(~"hee")));
                        match *items.get(1) {
                            ~ListItem(ref items) => {
                                assert_eq!(items.len(), 1);
                                assert_eq!(*items.get(0), ~AtomItem(StrAtom(~"hah")));
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
}
