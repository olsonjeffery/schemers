// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

use std::fmt::{Show, Formatter, Result};
use std::from_str::FromStr;
use std::num::One;
use num::rational::{Ratio, BigRational};
use num::bigint::BigInt;

use env::Env;
use eval::eval;
use result::SchemerResult;

pub type ExprResult = SchemerResult<Expr>;
pub type UnboxAndEvalResult = SchemerResult<(Vec<Expr>, Env)>;

#[deriving(PartialEq, Show, Clone)]
pub enum Expr {
    Atom(AtomVal),
    List(Vec<Box<Expr>>)
}

#[deriving(PartialEq, Show, Clone)]
pub enum AtomVal {
    Symbol(String),
    Integer(BigInt),
    Float(BigRational),
    Lambda(LambdaVal),
    Boolean(bool)
}

#[deriving(Clone)]
pub enum LambdaVal {
    UserDefined(String, Vec<String>, Box<Expr>),
    BuiltIn(String, fn(args: Vec<Expr>, env: Env) -> (Option<Expr>, Env))
}

impl LambdaVal {
    pub fn print(&self) -> String {
        match self {
            &UserDefined(ref name, ref vars, _) => {
                let var_expr = List(vars.iter().map(|v| box Atom(Symbol(v.to_string()))).collect());
                format!("lambda:{}{}", name.to_string(), var_expr.print())
            },
            &BuiltIn(ref name, _) =>
                format!("builtin-fn:{}", name.to_string())
        }
    }
}

impl PartialEq for LambdaVal {
    // Our custom eq allows numbers which are near each other to be equal! :D
    fn eq(&self, other: &LambdaVal) -> bool {
        match self {
            &UserDefined(ref name, ref args, ref body) => match other {
                &UserDefined(ref other_name, ref other_args, ref other_body) =>
                    name == other_name &&
                {
                    if args.len() != other_args.len() {
                        return false;
                    }
                    let mut args_match = true;
                    for ctr in range(0, args.len()) {
                        args_match = args.get(ctr)
                            == other_args.get(ctr);
                    }
                    args_match
                } &&
                    body == other_body,
                &BuiltIn(_, _) => false
            },
            &BuiltIn(ref name, ref body_fn) => match other {
                &BuiltIn(ref other_name, ref other_body_fn) =>
                    *body_fn as *u8 == *other_body_fn as *u8 && name == other_name,
                &UserDefined(_, _, _) => false
            }
        }
    }
}

impl Show for LambdaVal {
    fn fmt(&self, f: &mut Formatter) -> Result {
        // The `f.buf` value is of the type `&mut io::Writer`, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        write!(f, "{}", self.print())
    }
}

impl Expr {
    pub fn new_atom(input: String) -> ExprResult {
        let first_char = input.as_slice().char_at(0);
        match first_char {
            '0' | '1' | '2' | '3' | '4' | '5' |
            '6' | '7' | '8' | '9' => {
                match input.as_slice().contains(".") {
                    true => {
                        let parsed_val: Option<f64>
                            = FromStr::from_str(input.as_slice());
                        match parsed_val {
                            Some(val) => Ok(Atom(Number::float(val))),
                            None => Err(format!("Cannot parse number-like input of: '{}'",
                                                input))
                        }
                    },
                    false => {
                        match FromStr::from_str(input.as_slice()) {
                            Some(val) => Ok(Atom(Integer(val))),
                            None => Err(format!("Cannot parse number-like input of: {}",
                                                input))
                        }
                    }
                }
            },
            '#' => {
                // #-prefix parsing
                match &input {
                    v if *v == "#f".to_string() => Ok(Atom(Boolean(false))),
                    v if *v == "#false".to_string() => Ok(Atom(Boolean(false))),
                    v if *v == "#t".to_string() => Ok(Atom(Boolean(true))),
                    v if *v == "#true".to_string() => Ok(Atom(Boolean(true))),
                    _ => Err(format!("un-implemented case of sharp/pound-prefixing"))
                }
            }
            _ => Ok(Atom(Symbol(input)))
        }
    }

    pub fn cons(car: Expr, cdr: Expr) -> ExprResult {
        let mut new_items = vec!(box car);
        match cdr {
            v @ Atom(_) => new_items.push(box v),
            List(cdr_items) => for i in cdr_items.move_iter() {
                new_items.push(i);
            }
        }
        Ok(List(new_items))
    }

    pub fn print(&self) -> String {
        match self {
            &List(ref items) => {
                let out = items.iter().map(|i| i.print())
                    .fold("(".to_string(), |m, v| m.append(v.append(" ").as_slice()));
                out.as_slice().trim().to_string().append(")")
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

    // type-evaluating built-ins
    pub fn is_atom(&self) -> bool {
        match *self {
            Atom(_) => true,
            _ => false
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            List(ref items) => items.len() == 0,
            _ => false
        }
    }

    // fns for consuming/breaking down Exprs into composed elements
    // or underlying values
    pub fn unbox_and_eval(self, env: Env) -> UnboxAndEvalResult {
        match self {
            List(items) => {
                let args: Vec<Expr> = items.move_iter().map(|x| *x).collect();
                let mut env = env;
                let mut evald_args = Vec::new();
                for i in args.move_iter() {
                    let (evald_arg, out_env) = eval(i, env);
                    let evald_arg =
                        evald_arg
                        .expect("eval'd arg should ret expr");
                    env = out_env;
                    evald_args.push(evald_arg);
                }
                Ok((evald_args, env))
            },
            _ => Err(format!("calling unbox on non-List expr"))
        }
    }
    pub fn unwrap_float(self) -> BigRational {
        match self {
            Atom(Integer(v)) => Ratio::new(v, One::one()),
            Atom(Float(v)) => v,
            _ => fail!("calling unwrap_float() on non-numeric value")
        }
    }
    pub fn unwrap_integer(self) -> BigInt {
        match self {
            Atom(Float(v)) => v.numer().clone(),
            Atom(Integer(v)) => v,
            _ => fail!("calling unwrap_integer() on non-numeric value")
        }
    }
}

impl AtomVal {
    pub fn print(&self) -> String {
        match self {
            &Symbol(ref v) => v.to_string(),
            &Integer(ref v) => v.to_str(),
            &Float(ref v) => Number::float_print(v),
            &Lambda(ref v) => v.print(),
            &Boolean(ref v) => "#".to_string().append(format!("{}", v).as_slice())
        }
    }
}

pub mod Number {
    use super::{AtomVal, Integer, Float};
    use num::bigint::BigInt;
    use num::rational::{Ratio, BigRational};
    use collections::TreeSet;
    use std::char;
    use std::num::Zero;
    #[allow(dead_code)]
    pub fn integer(val: i64) -> AtomVal {
        let bi: BigInt = FromPrimitive::from_i64(val)
            .expect("Number::integer: should be able to unwrap i64->BigInt");
        Integer(bi)
    }
    pub fn uint(val: uint) -> AtomVal {
        let bi: BigInt = FromPrimitive::from_uint(val)
            .expect("Number::uint: should be able to unwrap uint->BigInt");
        Integer(bi)
    }
    pub fn float(val: f64) -> AtomVal {
        let br = Ratio::from_float(val)
            .expect("Number::float: should be able to unwrap f64->BigInt");
        Float(br)
    }
    // adapted from https://gist.github.com/kballard/4771f0d338fbb1896446
    pub fn float_print(v: &BigRational) -> String {
        let mut s = v.to_integer().to_str();
        let mut v = v.fract();
        if !v.is_zero() {
            s.push_char('.');
            let ten: BigInt = FromPrimitive::from_int(10).unwrap();
            let ten = Ratio::from_integer(ten);
            let mut seen = TreeSet::new();
            while !v.is_zero() {
                v = v * ten;
                if seen.contains(&v) {
                    s.push_str("…");
                    break;
                }
                seen.insert(v.clone());
                let digit = v.to_integer(); // should only be 1 digit
                // bail out when we hit a zero in the decimal portion
                if digit.is_zero() {
                    break;
                }
                s.push_char(char::from_digit(digit.to_uint().unwrap(), 10).unwrap());
                v = v.fract();
            }
        }
        s.into_string()
    }
}
