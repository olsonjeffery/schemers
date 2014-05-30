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

pub mod eval;
pub mod parse;
pub mod expr;
pub mod env;
pub mod builtins;
#[cfg(test)]
mod test;

#[cfg(not(test))]
fn main() {
    use std::os::args;
    let args: Vec<~str> = args().move_iter().skip(1).collect();
    if args.len() == 0 {
        fail!("no program provided");
    }
    let program: ~str = args.move_iter()
        .fold("".to_owned(), |memo, arg| memo + arg + " ").trim().to_owned();
    let env = builtins::add_builtins(env::Env::new(None, None, None));
    match eval::eval(parse::read(program), env) {
        (Some(expr), _) => println!("=> {}", expr.print()),
        _ => {}
    }
}
