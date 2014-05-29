// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use collections::hashmap::HashMap;
use expr::Expr;

pub struct Env {
    entries: HashMap<~str, Expr>,
    outer: Option<~Env>
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
