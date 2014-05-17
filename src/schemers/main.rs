// Copyright 2013-2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
#![crate_id="http://olsonjeffery.github.io#schemers:0.0.1"]
#![desc = "Simple Scheme Interpreter"]
#![license = "3-Clause BSD"]

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

pub enum ParseItem {
    Atom(~str),
    List(Vec<~ParseItem>)
}

fn read(tokens: &mut Vec<~str>) -> ParseItem {
    println!("current state of tokens: '{:?}'", tokens);
    match tokens.pop().expect("calling read() w/ empty token list; shouldn't happen") {
        ref x if *x == ~"(" => {
            let mut list = Vec::new();
            while *tokens.get(0) != ~")" {
                list.push(box read(tokens));
            }
            tokens.pop();
            List(list)
        },
        ref x if *x == ~")" => fail!("hit ) token; shouldn't happen"),
        x => Atom(x)
    }
}

#[cfg(test)]
mod parser_test {
    use super::{pad_input, tokenize, read, Atom};

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
    fn parse_should_take_a_vector_consisting_an_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize(~"bar");
        let parsed_item = read(tokens);
        println!("{:?}", parsed_item);
        match parsed_item {
            Atom(val) => {
                assert_eq!(val, ~"bar")
            },
            _ => assert!(false)
        }
    }
}
