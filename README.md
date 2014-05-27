# scheme.rs -- A toy Scheme implementation in Rust

This repository houses a self-contained implementation of Peter Norvig's [first essay on a simple Scheme implementation in Python][Norvig1] (but in Rust, of course). It is not particularly well-factored or optimized, but <s>was</s> is meant as a learning exercise in interpreter implementation. In this regard, it continues to be instructive.

The codebase is well tested, with many examples demonstrating the use of all of the constructs contained herein.

## TODO List

In no particular order:

#### Cleanup/completeness

* Work on the actual API:
  * It is quite hideous right now, with `fail!`s all over the place
  * Should replace with `Result`-based API (would definitely ease error-reporting centralization, as well as improving overall grossness)
  * The current implementation is heavily entrenched in pass-by-value semantics; it'd be real swell to get the desired behavior with a lot more use of references, slices, etc (I fell-back to by-val after hitting several walls w/ referenced-based approaches)
* Implement a proper REPL
* Port to a newer version of Rust (waiting for `~str` removal to land)
* Refactor code layout:
  * It's all one big file with impl plus tests (closing in on a 1,000 line, ewww)
  * Split the project into lib and bin projects
  * Probably drop tests into their own module so a LOC grep can just hit the impl modules
  * __API Documentation__

#### Feature Work

* Tackle the contents Peter Norvig's [second essay on the same topic][Norvig2], notably:
  * More `Atom` types (strings, <s>bools</s>, complex numbers, etc)
  * Macros
  * TCO
  * Better error detection/parsing
  * Expanded list of primitive procedures
  * The expanded test suite (would probably just adapt `lispytest.py` for this)

[Norvig1]: http://norvig.com/lispy.html "(How to Write a (Lisp) Interpreter (in Python))"
[Norvig2]: http://norvig.com/lispy2.html "(An ((Even Better) Lisp) Interpreter (in Python))"
