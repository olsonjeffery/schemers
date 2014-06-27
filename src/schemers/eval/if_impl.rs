use super::{EvalResult, eval};
use expr::{Expr, Atom, Boolean, List};
use env::Env;

macro_rules! try_opt(
    ($e:expr, $msg:expr) => (match $e {
        Some(e) => e,
        None => return Err($msg) })
)

pub fn eval_if(cdr: Expr, env: Env) -> EvalResult {
    match cdr {
        List(mut items) => {
            if items.len() != 3 {
                return Err("eval: if: should be three entries in if cdr list".to_string());
            }
            let first_arg = *try_opt!(items.shift(),
                "eval: if: should have some val in arg first position".to_string());
            let (out_expr, out_env) = try!(eval(
                first_arg, env));
            match out_expr {
                Some(Atom(Boolean(false))) => {
                    let consq_branch = *try_opt!(items.pop(),
                                                "eval: if: got none in conseq branch".to_string());
                    eval(consq_branch, out_env)
                },
                _ => eval(*try_opt!(items.shift(),
                                   "eval: if: got none in else branch".to_string()),
                          out_env)
            }
        },
        _ => return Err("eval: if: should have List in cdr position".to_string())
    }
}
