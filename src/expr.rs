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
pub type UnConsResult = SchemerResult<(Expr, Expr)>;
pub type PrintResult = SchemerResult<String>;

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
    UserDefined(String, Vec<String>, Box<Expr>, Env),
    BuiltIn(String, fn(args: Vec<Expr>, env: Env) -> SchemerResult<(Option<Expr>, Env)>)
}

impl LambdaVal {
    pub fn print(&self) -> PrintResult {
        match self {
            &UserDefined(ref name, ref vars, _, _) => {
                let var_expr = List(vars.iter().map(|v| box Atom(Symbol(v.to_string()))).collect());
                let printed_val = match var_expr.print() {
                    Ok(v) => v,
                    Err(e) => return Err(e)
                };
                Ok(format!("lambda:{}{}", name.to_string(), printed_val))
            },
            &BuiltIn(ref name, _) =>
                Ok(format!("builtin-fn:{}", name.to_string()))
        }
    }
}

impl PartialEq for LambdaVal {
    // Our custom eq allows numbers which are near each other to be equal! :D
    fn eq(&self, other: &LambdaVal) -> bool {
        match self {
            &UserDefined(ref name, ref args, ref body, _) => match other {
                &UserDefined(ref other_name, ref other_args, ref other_body, _) =>
                    name == other_name &&
                {
                    if args.len() != other_args.len() {
                        return false;
                    }
                    let mut args_match = true;
                    for ctr in range(0, args.len()) {
                        args_match = args[ctr]
                            == other_args[ctr];
                    }
                    args_match
                } &&
                    body == other_body,
                &BuiltIn(_, _) => false
            },
            &BuiltIn(ref name, ref body_fn) => match other {
                &BuiltIn(ref other_name, ref other_body_fn) =>
                    *body_fn as *mut u8 == *other_body_fn
                        as *mut u8 && name == other_name,
                &UserDefined(_, _, _, _) => false
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
                            Some(val) => {
                                let new_float = match Number::float(val) {
                                    Ok(v) => v,
                                    Err(e) => return Err(e)
                                };
                                Ok(Atom(new_float))
                            },
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

    pub fn print(&self) -> PrintResult {
        match self {
            &List(ref items) => {
                // this has to change
                let mut items_iter = items.iter();
                let mut has_more = true;
                let mut printed_val = "(".to_string();
                while has_more {
                    match items_iter.next() {
                        None => has_more = false,
                        Some(ref i) => match i.print() {
                            Ok(i) => {
                                printed_val = printed_val.append(i.append(" ").as_slice())
                            },
                            Err(e) => return Err(e)
                        }
                    }
                }
                Ok(printed_val.as_slice().trim().to_string().append(")"))
            },
            &Atom(ref v) => (*v).print()
        }
    }
    pub fn un_cons(self) -> UnConsResult {
        match self {
            Atom(_) => return Err("expr.un_cons(): Cannot un_cons an Atom value"
                                  .to_string()),
            List(mut items) => {
                if items.len() == 0 {
                    return Err("expr.un_cons(): cannot build car/cdr of empty list"
                               .to_string());
                }
                let car = match items.remove(0) {
                    Some(v) => v,
                    None => return Err("expr.un_cons(): items collection empty; \
                        shouldn't happen".to_string())
                };
                Ok((*car, List(items)))
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
                    let (evald_arg, out_env) = try!(eval(i, env));
                    let evald_arg = match evald_arg {
                        Some(v) => v,
                        _ => return Err("eval'd arg should ret expr".to_string())
                    };
                    env = out_env;
                    evald_args.push(evald_arg);
                }
                Ok((evald_args, env))
            },
            _ => Err(format!("calling unbox on non-List expr"))
        }
    }
    pub fn into_float(self) -> SchemerResult<BigRational> {
        match self {
            Atom(Integer(v)) => Ok(Ratio::new(v, One::one())),
            Atom(Float(v)) => Ok(v),
            _ => Err("calling into_float() on non-numeric value".to_string())
        }
    }
    pub fn into_integer(self) -> SchemerResult<BigInt> {
        match self {
            Atom(Float(v)) => Ok(v.numer().clone()),
            Atom(Integer(v)) => Ok(v),
            _ => Err("calling into_integer() on non-numeric value".to_string())
        }
    }
}

impl AtomVal {
    pub fn print(&self) -> PrintResult {
        match self {
            &Symbol(ref v) => Ok(v.to_string()),
            &Integer(ref v) => Ok(v.to_string()),
            &Float(ref v) => match Number::float_print(v) {
                Ok(v) => Ok(v),
                Err(e) => return Err(e)
            },
            &Lambda(ref v) => v.print(),
            &Boolean(ref v) => Ok("#".to_string().append(format!("{}", v).as_slice()))
        }
    }
}

pub mod Number {
    use std::char;
    use std::num::Zero;

    use num::bigint::BigInt;
    use num::rational::{Ratio, BigRational};
    use collections::TreeSet;

    use result::SchemerResult;
    use super::{AtomVal, Integer, Float};
    use super::PrintResult;
    #[allow(dead_code)]
    pub fn integer(val: i64) -> SchemerResult<AtomVal> {
        let bi: BigInt = match FromPrimitive::from_i64(val) {
            Some(v) => v,
            None => return Err("Number::integer: should be able to convert i64->BigInt".to_string())
        };
        Ok(Integer(bi))
    }
    pub fn uint(val: uint) -> SchemerResult<AtomVal> {
        let bi: BigInt = match FromPrimitive::from_uint(val) {
            Some(v) => v,
            None => return Err("Number::uint: should be able to convert uint->BigInt".to_string())
        };
        Ok(Integer(bi))
    }
    pub fn float(val: f64) -> SchemerResult<AtomVal> {
        let br = match Ratio::from_float(val) {
            Some(v) => v,
            None => return Err("Number::float: should be able to convert f64->BigInt".to_string())
        };
        Ok(Float(br))
    }
    // adapted from https://gist.github.com/kballard/4771f0d338fbb1896446
    pub fn float_print(v: &BigRational) -> PrintResult {
        let mut s = v.to_integer().to_string();
        let mut v = v.fract();
        if !v.is_zero() {
            s.push_char('.');
            let ten: BigInt = match FromPrimitive::from_int(10) {
                Some(v) => v,
                None => return Err("float_print: unable to parse 10. Highly unlikely.".to_string())
            };
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
                let digit = match digit.to_uint() {
                    Some(v) => v,
                    None => return Err("float_print: failure to print digit".to_string())
                };
                let char_digit = match char::from_digit(digit, 10) {
                    Some(v) => v,
                    None => return Err(
                        "float_print: couldn't parse char_digit. Also unlikely.".to_string())
                };
                s.push_char(char_digit);
                v = v.fract();
            }
        }
        Ok(s.into_string())
    }
}
