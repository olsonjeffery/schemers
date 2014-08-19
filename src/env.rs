// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use std::collections::hashmap::HashMap;
use expr::{Expr, ExprResult};
use result::SchemerResult;

pub type EnvResult = SchemerResult<Env>;
pub type EnvSetResult = SchemerResult<()>;

#[deriving(Clone)]
pub struct Env {
    entries: HashMap<String, Expr>,
    outer: Option<Box<Env>>
}

impl Env {
    pub fn new_empty() -> Env {
        Env {
            entries: HashMap::new(),
            outer: None
        }
    }
    pub fn new(
        params: Option<Vec<String>>,
        args: Option<Vec<Expr>>,
        outer: Option<Env>) -> EnvResult {
        let mut entries = HashMap::new();
        if params.is_some() && args.is_some() {
            let params = match params {
                Some(p) => p,
                None => return Err("Env::new(): params should be a value".to_string())
            };
            let args = match args {
                Some(p) => p,
                None => return Err("Env::new(): args should be a value".to_string())
            };
            if params.len() == args.len() {
                for ctr in range(0,params.len()) {
                    let var_name = params[ctr].to_string();
                    let arg = args[ctr].clone();
                    entries.insert(var_name, arg);
                }
            } else {
                return Err("Env::new(): params and args length doesn't match".to_string())
            }
        }
        else if params.is_none() && args.is_none() {
            if outer.is_none() {
                return Ok(Env::new_empty())
            }
        } else {
            return Err("Env::new(): cannot have params & args unset".to_string())
        }
        Ok(Env { entries: entries, outer: match outer { Some(e) => Some(box e), None => None } })
    }

    pub fn set(&mut self, symbol: String, val: Expr) -> EnvSetResult {
        match self.entries.contains_key(&symbol) {
            true => {
                self.define(symbol, val);
                Ok(())
            },
            false => match self.outer {
                Some(ref mut outer_env) =>
                    outer_env.set(symbol, val),
                None => Err(
                    format!("Env.set(): Symbol '{}' is not defined in this Env scope chain",
                            symbol))
            }
        }
    }

    pub fn define(&mut self, symbol: String, val: Expr) {
        self.entries.insert(symbol, val);
    }

    pub fn find<'b>(&'b self, symbol: &String) -> ExprResult {
        match self.entries.find(symbol) {
            Some(v) => Ok(v.clone()),
            None => {
                match &self.outer {
                    &Some(ref outer_env) => outer_env.find(symbol),
                    &None => Err(format!("No variable named {} defined in the environment."
                                   , *symbol))
                }
            }
        }
    }

    pub fn enclose(&mut self, base: Env) {
        match self.outer {
            Some(ref mut outer) => outer.enclose(base),
            None => self.outer = Some(box base)
        }
    }

    pub fn into_parent(self) -> Option<Env> {
        self.outer.map(|x| *x)
    }
}
