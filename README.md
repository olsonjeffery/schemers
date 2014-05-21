# scheme.rs -- A toy Scheme implementation in Rust

This repository houses a self-contained implementation of Peter Norvig's [first essay on a simple Scheme implementation in Python][Norvig1] (but in Rust, of course). It is not particularly well-factored or optimized, but was meant as a learning exercise in interpreter implementation. In this regard, it was successful.

The codebase is well tested, with many examples demonstrating the use of all of the constructs contained herein.

### TODO List

In no particular order:

* Implement the contents of `add_globals`
* Refactor code layout:
  * It's all one big file with impl plus tests (closing in on a 1,000 line, ewww)
  * Probably drop tests into their own module so a LOC grep can just hit the impl modules
* Port to a newer version of Rust (waiting for `~str` removal to land)
* Add the test cases that appear near the env of [the first essay][Norvig1]
* Tackle the contents Peter Norvig's [second essay on the same topic][Norvig2], notably:
  * more `Atom` types (strings, bools, complex numbers, etc)
  * Macros
  * TCO
  * Better error detection/parsing
  * Expanded list of primitive procedures
  * The expanded test suite (would probably just adapt `lispytest.py` for this)

[Norvig1]: http://norvig.com/lispy.html "(How to Write a (Lisp) Interpreter (in Python))"
[Norvig2]: http://norvig.com/lispy2.html "(An ((Even Better) Lisp) Interpreter (in Python))"
