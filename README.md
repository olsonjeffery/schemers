# scheme.rs -- A toy Scheme implementation in Rust

![travis-ci.org](https://travis-ci.org/olsonjeffery/schemers.svg?branch=master)

This repository houses a self-contained implementation of Peter Norvig's [first essay on a simple Scheme implementation in Python][Norvig1] (but in Rust, of course). It is not particularly well-factored or optimized, but <s>was</s> is meant as a learning exercise in interpreter implementation. In this regard, it continues to be instructive.

The codebase is well tested, with many examples demonstrating the use of all of the constructs contained herein.

## Upcoming Work

* TCO
  * `fact(100)` in the `norvig_suite` is currently disabled. This obviously needs to be enabled again (and work!)
* Hygenic Macros (do want `let`)
* Some benchmarks:
  * `fact(1000)` (can't be done before TCO lands)
  * Some kind of `Env`-copy/`lambda`-centric benchmark to get data before working on `Env`/`Expr`-passing refactor below
* Move away from by-val `Expr` passing for *everything* in the API.
  * `Env` will store just `Rc<Expr>`, instead of `Expr`
  * `eval` will take `Rc<Expr>`s, as well as all of the helpers that fall out from it
  * In this way, the boxing is bottlenecked to boundary between `read` and `eval`
  * Not worried about ensuring that there's only one instance of a given `Expr` value in all of the `Env`s; just that there's a net decrease `in Expr` value copies
  * Probably has test fallout (can capture w/i `test_eval!`?)
* Build-scripting to guarantee against the reintroduction of `fail!`, `unwrap` and `expect` (`tidy.py` work? also `println!`?)
* __API Documentation__
* `result::SchemerResult` should change its `Err` component from `String` to take a new `SchemerError` value
  * `SchemerError` contains the module/type/method/function path of the `Err`-ing code (wish this could be handled in a macro), along with the `String` error component
  * This enables a more uniform error display in the REPL/elsewhere
  * Generating the `Err` variant of `SchemerError` can be handled in a macro, and all makers of `SchemerResult` will be caught up in this change
* Tackle the (remaining) contents Peter Norvig's [second essay on the same topic][Norvig2], notably:
  * More `Atom` types (strings, <s>bools</s>, complex numbers, etc)
  * Better error detection/parsing
  * Expanded list of primitive procedures
  * The expanded test suite (would probably just adapt `lispytest.py` for this)
* Implement a proper REPL

[Norvig1]: http://norvig.com/lispy.html "(How to Write a (Lisp) Interpreter (in Python))"
[Norvig2]: http://norvig.com/lispy2.html "(An ((Even Better) Lisp) Interpreter (in Python))"

