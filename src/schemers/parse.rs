// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, List};

pub fn read(input: ~str) -> Expr {
    parse(&mut tokenize(input))
}

pub fn pad_input(input: ~str) -> ~str {
    input.replace("(", " ( ").replace(")", " ) ")
        .replace("\t", " ").replace("\n", " ").replace("\r", " ")
}

fn strip_commas(input: ~str) -> ~str {
    input.replace(",", " ").replace("  ", " ")
}

pub fn tokenize(input: ~str) -> Vec<~str> {
    strip_commas(pad_input(input)).split_str(" ")
        .filter(|i| *i != "").map(|i| i.to_owned()).collect()
}

pub fn parse(tokens: &mut Vec<~str>) -> Expr {
    let current_token =
        tokens.shift().expect("calling parse() w/ empty token list; shouldn't happen");
    match current_token {
        ref x if *x == "(".to_owned() => {
            let mut list = Vec::new();
            while *tokens.get(0) != ")".to_owned() {
                list.push(box parse(tokens));
            }
            tokens.shift();
            List(list)
        },
        ref x if *x == ")".to_owned() => fail!("hit ) token; shouldn't happen"),
        x => Expr::new_atom(x)
    }
}
