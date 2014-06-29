// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
#![crate_id="http://olsonjeffery.github.io#schemers:0.0.1"]
#![desc = "Simple Scheme Interpreter"]
#![license = "3-Clause BSD"]
#![feature(macro_rules)]

extern crate collections = "collections";
extern crate num = "num";

pub mod eval;
pub mod parse;
pub mod expr;
pub mod env;
pub mod builtins;
pub mod result;
#[cfg(test)]
mod test;

/*
#[cfg(not(test))]
fn main() {
    use std::os::args;
    let args: Vec<String> = args().move_iter().skip(1).collect();
    if args.len() == 0 {
        fail!("no program provided");
    }
    let program: String = args.move_iter()
        .fold("".to_string(), |memo, arg| memo.append(arg.as_slice())
              .append(" ".as_slice())).as_slice()
        .trim().to_string();
    let env = builtins::add_builtins(env::Env::new_empty());
    let expr = match parse::read(program) {
        Ok(expr) => expr,
        Err(err) => fail!("ERROR: {}", err)
    };
    match eval::eval(expr, env) {
        Ok((Some(expr), _)) => match expr.print() {
            Ok(expr) =>
                println!("=> {}", expr),
            Err(e) => println!("ERROR: {}", e)
        },
        _ => {}
    }
}
*/
