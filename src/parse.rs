// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use expr::{Expr, List, ExprResult};

pub fn read(input: String) -> ExprResult {
    parse(&mut tokenize(input))
}

pub fn pad_input(input: String) -> String {
    input.replace("(", " ( ").replace(")", " ) ")
        .replace("\t", " ").replace("\n", " ").replace("\r", " ")
}

fn strip_commas(input: String) -> String {
    input.replace(",", " ").replace("  ", " ")
}

pub fn tokenize(input: String) -> Vec<String> {
    strip_commas(pad_input(input)).as_slice().split_str(" ")
        .filter(|i| *i != "".as_slice()).map(|i| i.to_string()).collect()
}

pub fn parse(tokens: &mut Vec<String>) -> ExprResult {
    let current_token = match tokens.shift() {
        Some(t) => t,
        None => return Err("parse: calling w/ empty token list; shouldn't happen".to_string())
    };
    match current_token {
        ref x if *x == "(".to_string() => {
            let mut list = Vec::new();
            while *tokens.get(0) != ")".to_string() {
                match parse(tokens) {
                    Ok(expr) => list.push(box expr),
                    err => return err
                }
            }
            tokens.shift();
            Ok(List(list))
        },
        ref x if *x == ")".to_string() =>
            Err("hit ) token; shouldn't happen".to_string()),
        x => Expr::new_atom(x)
    }
}
