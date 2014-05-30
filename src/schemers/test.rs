// Copyright 2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.

mod parser_tests {
    use parse::{pad_input, tokenize, parse, read};
    use expr::{Atom, List, Symbol,
                Boolean, Number};

    #[test]
    fn pad_input_should_insert_spaces_before_and_after_parens() {
        assert_eq!(pad_input("(x 1 2 3)".to_owned()), " ( x 1 2 3 ) ".to_owned());
    }

    #[test]
    fn tokenize_string_into_tokens() {
        let tokens = tokenize("(foo 12 3)".to_owned());
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens.get(0).to_owned(), "(".to_owned());
        assert_eq!(tokens.get(1).to_owned(), "foo".to_owned());
        assert_eq!(tokens.get(2).to_owned(), "12".to_owned());
        assert_eq!(tokens.get(3).to_owned(), "3".to_owned());
        assert_eq!(tokens.get(4).to_owned(), ")".to_owned());
    }

    #[test]
    fn should_parse_tokens_consisting_of_a_str_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize("bar".to_owned());
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(Symbol(val)) => {
                assert_eq!(val, "bar".to_owned())
            },
            _ => assert!(false)
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_an_int_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize("42".to_owned());
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(val) => {
                assert_eq!(val, Number::integer(42))
            },
            _ => assert!(false)
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_a_float_atom_and_convert_it_to_a_parse_item() {
        let tokens = &mut tokenize("1.1".to_owned());
        let parsed_item = parse(tokens);
        match parsed_item {
            Atom(val) => {
                assert_eq!(val, Number::float(1.1))
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn should_parse_tokens_consisting_of_a_list_of_atoms_and_parse_it() {
        let tokens = &mut tokenize("(bar 12 45)".to_owned());
        let parsed_item = parse(tokens);
        assert_eq!(parsed_item.is_atom(), false);
        match parsed_item {
            List(items) => {
                let items_len = items.len();
                assert!(3 == items_len);
                assert_eq!(items.get(0).is_atom(), true);
                assert_eq!(*items.get(0), box Atom(Symbol("bar".to_owned())));
                assert_eq!(items.get(1).is_atom(), true);
                assert_eq!(*items.get(1), box Atom(Number::integer(12)));
                assert_eq!(items.get(2).is_atom(), true);
                assert_eq!(*items.get(2), box Atom(Number::integer(45)));
            },
            _ => fail!("got back an atom, it seems")
        }
    }
    #[test]
    fn should_parse_tokens_consisting_of_a_list_of_atoms_and_a_nested_list() {
        let tokens = &mut tokenize("((2 3) bar 12 (hee (hah)))".to_owned());
        let parsed_item = parse(tokens);
        assert_eq!(parsed_item.is_atom(), false);
        match parsed_item {
            List(items) => {
                let items_len = items.len();
                assert!(4 == items_len);
                match *items.get(0) {
                    box List(ref items) => {
                        assert_eq!(items.len(), 2);
                        assert_eq!(*items.get(0), box Atom(Number::integer(2)));
                        assert_eq!(*items.get(1), box Atom(Number::integer(3)));
                    },
                    _ => fail!("shoulda got a list")
                }
                assert_eq!(*items.get(1), box Atom(Symbol("bar".to_owned())));
                assert_eq!(*items.get(2), box Atom(Number::integer(12)));
                match *items.get(3) {
                    box List(ref items) => {
                        assert_eq!(items.len(), 2);
                        assert_eq!(*items.get(0), box Atom(Symbol("hee".to_owned())));
                        match *items.get(1) {
                            box List(ref items) => {
                                assert_eq!(items.len(), 1);
                                assert_eq!(*items.get(0), box Atom(Symbol("hah".to_owned())));
                            },
                            _ => fail!("shoulda got a list")
                        }
                    },
                    _ => fail!("shoulda got a list")
                }
            },
            _ => fail!("got back an atom, it seems")
        }
    }
    #[test]
    fn parsed_tokens_of_a_list_of_atoms_and_a_nested_list_should_to_str_correctly() {
        let input = "((2 3) bar 12 (hee (hah)))".to_owned();
        let tokens = &mut tokenize(input.to_owned());
        let parsed_item = parse(tokens);
        let output = parsed_item.print();
        assert_eq!(input, output);
    }
    #[test]
    fn commas_should_be_treated_as_whitespace() {
        let expr = read("(x, y,z)".to_owned());
        match &expr {
            &List(ref items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items.get(0), &box Atom(Symbol("x".to_owned())));
                assert_eq!(items.get(1), &box Atom(Symbol("y".to_owned())));
                assert_eq!(items.get(2), &box Atom(Symbol("z".to_owned())));
            }, _ => fail!("expected a list")
        }
        assert_eq!(expr.print(), "(x y z)".to_owned())
    }
    #[test]
    fn can_parse_boolean_literals() {
        let false_atom = read("#f".to_owned());
        assert_eq!(false_atom, Atom(Boolean(false)));
        let true_atom = read("#t".to_owned());
        assert_eq!(true_atom, Atom(Boolean(true)));
        let false_atom = read("#false".to_owned());
        assert_eq!(false_atom, Atom(Boolean(false)));
        let true_atom = read("#true".to_owned());
        assert_eq!(true_atom, Atom(Boolean(true)));
    }
}

mod eval_tests {
    use parse::{read};
    use builtins::{add_builtins};
    use expr::{Atom, List, Lambda, Symbol,
                UserDefined, Number};
    use env::Env;
    use eval::eval;

    #[should_fail]
    #[test]
    fn an_atom_expr_returns_the_atom_in_the_car_with_none_in_the_cdr() {
        let expr = read("x".to_owned());
        expr.un_cons();
    }

    #[test]
    fn a_list_with_one_atom_element_returns_an_atom_car_and_an_empty_list_in_the_cdr() {
        let expr = read("(x)".to_owned());
        let (car, cdr) = expr.un_cons();
        assert_eq!(car, Atom(Symbol("x".to_owned())));
        match cdr {
            List(items) => assert_eq!(items.len(), 0),
            _ => fail!("expected a list")
        }
    }

    #[test]
    fn a_list_with_multiple_elems_puts_the_first_the_car_and_the_rest_in_the_cdr() {
        let expr = read("(y 2 3)".to_owned());
        let (car, cdr) = expr.un_cons();
        assert_eq!(car, Atom(Symbol("y".to_owned())));
        match cdr {
            List(items) => {
                assert_eq!(items.len(), 2)
            },
            _ => fail!("expected a list")
        }
    }

    #[test]
    #[should_fail]
    fn calling_un_cons_on_an_empty_list_should_fail() {
        let expr = read("()".to_owned());
        expr.un_cons();
    }

    #[test]
    fn given_a_symbol_in_the_env_then_calling_eval_should_resolve_its_value() {
        let env = Env::new(
            Some(vec!("x".to_owned())),
            Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read("x".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.expect("should return an expr"),
                   Atom(Number::integer(42)));
    }

    #[test]
    #[should_fail]
    fn given_a_symbol_NOT_in_the_env_then_calling_eval_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read("x".to_owned());
        eval(in_expr, env);
    }

    #[test]
    fn given_a_float_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = read("34.3".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(34.3)));
    }

    #[test]
    fn given_a_integer_literal_then_calling_eval_should_return_it_back() {
        let env = Env::new(None, None, None);
        let in_expr = read("34".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(34)));
    }

    #[test]
    fn given_a_quote_of_an_atom_expr_should_resolve_to_just_the_atom() {
        let env = Env::new(None, None, None);
        let in_expr = read("(quote 42)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(42)));
    }

    #[test]
    #[should_fail]
    fn a_quote_with_no_params_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read("(quote)".to_owned());
        eval(in_expr, env);
    }

    #[test]
    fn given_a_quote_of_a_list_expr_it_should_resolve_to_the_list_without_resolving_symbols() {
        let env = Env::new(None, None, None);
        let in_expr = read("(quote (x 37))".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(*items.get(0), box Atom(Symbol("x".to_owned())));
                assert_eq!(*items.get(1), box Atom(Number::integer(37)));
            },
            _ => fail!("expected a list")
        }
    }

    #[test]
    fn only_an_empty_list_expr_should_have_is_null_return_true() {
        let empty_list = read("()".to_owned());
        assert_eq!(empty_list.is_null(), true);
        let not_null = read("(())".to_owned());
        assert_eq!(not_null.is_null(), false);
        let not_null = read("2".to_owned());
        assert_eq!(not_null.is_null(), false);
        let not_null = read("x".to_owned());
        assert_eq!(not_null.is_null(), false);
        let not_null = read("34.4".to_owned());
        assert_eq!(not_null.is_null(), false);
        let not_null = read("(x 3 4)".to_owned());
        assert_eq!(not_null.is_null(), false);
    }

    #[test]
    fn an_if_call_with_a_non_false_test_should_resolve_the_conseq_branch() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) =
            eval(read("(if (quote 1)(quote conseq) (quote alt))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol("conseq".to_owned())));
        let (out_expr, env) =
            eval(read("(if #t (quote conseq) (quote alt))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol("conseq".to_owned())));
        let (out_expr, _) =
            eval(read("(if (list)(quote conseq) (quote alt))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol("conseq".to_owned())));
    }

    #[test]
    fn an_if_call_with_a_false_test_should_resolve_the_alt_branch() {
        let env = Env::new(None, None, None);
        let in_expr = read("(if #f (quote conseq) (quote alt))".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol("alt".to_owned())));
    }

    #[test]
    fn calling_eval_with_an_empty_list_should_return_the_list_as_a_value() {
        let env = Env::new(None, None, None);
        let in_expr = read("()".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            list @ List(_) => {
                assert_eq!(list.is_null(), true);
            },
            _ => fail!("expect a list")
        }
    }

    #[test]
    #[should_fail]
    fn calling_set_on_a_var_not_in_the_scope_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read("(set! x 123)".to_owned());
        eval(in_expr, env);
    }

    #[test]
    fn calling_set_on_a_var_in_the_scope_should_succeed() {
        let env = Env::new(
            Some(vec!("x".to_owned())), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read("(set! x 123)".to_owned());
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(123)));
    }

    #[test]
    fn calling_set_should_resolve_the_val() {
        let env = Env::new(
            Some(vec!("x".to_owned())), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read("(set! x (quote 37))".to_owned());
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(37)));
    }

    #[test]
    fn calling_set_on_a_var_in_an_outer_scope_should_succeed() {
        let outer_env = Env::new(
            Some(vec!("x".to_owned())), Some(vec!(Atom(Number::integer(42)))),
            None);
        let env = Env::new(
            None, None,
            Some(outer_env));
        let in_expr = read("(set! x 43)".to_owned());
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(43)));
    }

    #[test]
    fn set_should_return_a_None_for_the_out_expr() {
        let env = Env::new(
            Some(vec!("x".to_owned())), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read("(set! x 43)".to_owned());
        let (result, _) = eval(in_expr, env);
        assert_eq!(result, None);
    }

    #[test]
    #[should_fail]
    fn calling_set_with_a_non_symbol_as_the_atom_should_fail() {
        let env = Env::new(None, None, None);
        let in_expr = read("(set! 1 123)".to_owned());
        eval(in_expr, env);
    }

    #[test]
    fn an_if_test_that_returns_a_None_out_expr_should_run_the_conseq_branch() {
        let env = Env::new(
            Some(vec!("x".to_owned())), Some(vec!(Atom(Number::integer(42)))),
            None);
        let in_expr = read("(if (set! x 37) (quote conseq) (quote alt))".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Symbol("conseq".to_owned())));
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(37)));
    }

    #[test]
    fn should_be_able_to_create_an_env_with_all_None_params() {
        Env::new(None, None, None);
    }

    #[test]
    fn evaling_a_define_should_return_None() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define x 123)".to_owned());
        let (result, _) = eval(in_expr, env);
        assert!(result.is_none());
    }

    #[test]
    fn defining_a_var_should_set_it_in_the_scope() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define x 123)".to_owned());
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(123)));
    }

    #[test]
    fn calling_define_should_resolve_the_val_of_the_input() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define x (quote foo))".to_owned());
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Symbol("foo".to_owned())));
    }

    #[test]
    fn defining_a_var_already_set_in_an_outer_scope_should_shadow_it_in_the_inner_scope() {
        let outer_env = Env::new(
            Some(vec!("x".to_owned())), Some(vec!(Atom(Number::integer(42)))),
            None);
        let env = Env::new(None, None, Some(outer_env));
        let in_expr = read("(define x 37)".to_owned());
        let (_, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(37)));
        let outer_env = env.unwrap_parent();
        assert_eq!(outer_env.find(&"x".to_owned()), Atom(Number::integer(42)));
    }

    #[test]
    fn lambda_exprs_should_expect_a_list_for_vars_and_an_expr_for_body() {
        let env = Env::new(None, None, None);
        let in_expr = read("(lambda (x) 37)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        match out_expr.unwrap() {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(_, vars, body) => {
                        assert_eq!(vars.len(), 1);
                        assert_eq!(vars.get(0), &"x".to_owned());
                        assert_eq!(*body, Atom(Number::integer(37)));
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
    }
    #[test]
    fn lambda_exprs_evald_outside_of_define_have_the_name_of_anonymous() {
        let env = Env::new(None, None, None);
        let in_expr = read("(lambda (x y) 37)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        let out_expr = out_expr.unwrap();
        let out_expr_as_str = out_expr.print();
        match out_expr {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(name, _, _) => {
                        assert_eq!(name, "anonymous".to_owned());
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
        assert_eq!(out_expr_as_str, "lambda:anonymous(x y)".to_owned());
    }
    #[test]
    fn lambda_exprs_evald_within_a_define_have_the_of_the_provided_var_symbol() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define foo (lambda (x y) 37))".to_owned());
        let (_, env) = eval(in_expr, env);
        let out_expr = env.find(&"foo".to_owned());
        let out_expr_as_str = out_expr.print();
        match out_expr {
            Atom(Lambda(body)) => {
                match body {
                    UserDefined(name, _, _) => {
                        assert_eq!(name, "foo".to_owned());
                    }, _ => fail!("expected a UserDefined")
                }
            }, _ => fail!("expected atom w/ lambda")
        }
        assert_eq!(out_expr_as_str, "lambda:foo(x y)".to_owned());
    }
    #[test]
    #[should_fail]
    fn lambda_expr_eval_should_fail_if_a_list_isnt_in_the_vars_position() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define foo (lambda x 37))".to_owned());
        eval(in_expr, env);
    }
    #[test]
    #[should_fail]
    fn lambda_expr_eval_should_fail_if_theres_no_third_position() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define foo (lambda (x)))".to_owned());
        eval(in_expr, env);
    }

    #[test]
    fn begin_should_return_the_evald_atom_expr_of_the_tail_param() {
        let env = Env::new(None, None, None);
        let in_expr = read("(begin (define x 37) 42)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(37)));
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(42)));
    }
    #[test]
    fn begin_should_return_no_expr_if_its_tail_param_returns_nothing() {
        let env = Env::new(None, None, None);
        let in_expr = read("(begin (define x 37))".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(37)));
        assert_eq!(out_expr, None);
    }
    #[test]
    fn begin_can_have_a_list_in_the_tail_position() {
        let env = Env::new(None, None, None);
        let in_expr = read("(begin (define x 37) (quote (1 x 3.4)))".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(env.find(&"x".to_owned()), Atom(Number::integer(37)));
        match out_expr.unwrap() {
            List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(*items.get(0), box Atom(Number::integer(1)));
                assert_eq!(*items.get(1), box Atom(Symbol("x".to_owned())));
                assert_eq!(*items.get(2), box Atom(Number::float(3.4)));
            }, _ => fail!("expected a list")
        }
    }
    #[test]
    fn can_define_a_var_with_the_same_name_as_a_special_form_keyword_but_SF_is_still_usable() {
        let env = Env::new(None, None, None);
        let in_expr = read("(begin (define set! 42) (set! set! 37) set!)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(37)));
    }

    #[test]
    fn can_define_a_lambda_that_returns_an_atom_and_subsequently_invoke_it() {
        let env = Env::new(None, None, None);
        let in_expr = read("(define get-1 (lambda () 1))".to_owned());
        let (_, env) = eval(in_expr, env);
        let in_expr = read("(get-1)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(1)));
    }

    #[test]
    fn can_define_a_lambda_that_takes_an_arg_and_invoke_it() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read(
            "(begin (define double (lambda (x) (+ x x))) (double 3))".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(6)));
    }

    #[test]
    fn builtin_print_sanity_check() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("+".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap().print(), "builtin-fn:+".to_owned());
    }
}

mod builtins_tests {
    use parse::read;
    use builtins::add_builtins;
    use eval::eval;
    use env::Env;
    use expr::{Atom,
                Boolean, Number};
    #[test]
    fn two_plus_two_equals_four() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(+ 2 2)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(4)));
    }
    #[test]
    fn can_sum_an_arbitrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(+ 2 2 1 1 1 1 4)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(12)));
    }
    #[test]
    fn two_point_five_plus_two_equals_four_point_five() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(+ 2.5 2)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(4.5)));
    }
    #[test]
    fn four_minus_three_equals_one() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(- 4 3)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(1)));
    }
    #[test]
    fn four_point_five_minus_one_equals_equals_three_point_five() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(- 4.5 1)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(3.5)));
    }
    #[test]
    fn can_subtract_an_arbitrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(- 4.5 1 3)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(0.5)));
    }
    #[test]
    fn two_times_twenty_one_equals_42() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(* 2 21)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(42)));
    }
    #[test]
    fn can_multiply_an_aribtrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(* 4 5 3)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(60)));
    }
    #[test]
    fn two_times_one_point_five_equals_three() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(* 2 1.5)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(3.0)));
    }
    #[test]
    fn ten_divided_by_5_equals_2() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(/ 10 5)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(2)));
    }
    #[test]
    fn five_divided_by_two_point_five_equals_2() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(/ 5 2.5)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::float(2.0)));
    }
    #[test]
    fn can_divide_an_arbitrary_number_of_items() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(/ 100 10 5)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Number::integer(2)));
    }
    #[test]
    fn lt_works() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(< 2 4)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(< 5 1)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read("(< 1.1 1.2)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(< 1.1 2)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(< 1.0 1)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read("(< 1 1)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn gt_works() {
        let env = add_builtins(Env::new(None, None, None));
        let in_expr = read("(> 4 2)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(> 1 5)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read("(> 1.2 1.1)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(> 2 1.1)".to_owned());
        let (out_expr,env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(not #f)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(not (quote #f))".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let in_expr = read("(> 1.0 1)".to_owned());
        let (out_expr, env) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let in_expr = read("(> 1 1)".to_owned());
        let (out_expr, _) = eval(in_expr, env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn a_call_to_not_with_anything_that_isnt_false_returns_false() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) = eval(read("(not #t)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(not (quote ()))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(not 1)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(not 1.3)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(not (quote 1))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(not (lambda (x) x))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, _) = eval(read("(not (+ 1 1))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn gte_works() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) = eval(read("(>= 2 2)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read("(>= 2 2.0)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read("(>= 2 1.9)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read("(>= 1.9 2)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, _) = eval(read("(>= 1 2)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn lte_works() {
        let env = add_builtins(Env::new(None, None, None));
        let (out_expr, env) = eval(read("(<= 2 2)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read("(<= 2 2.0)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read("(<= 2 1.9)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(<= 1.9 2)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, _) = eval(read("(<= 1 2)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
    }
    #[test]
    #[should_fail]
    fn lt_fails_with_less_than_two_args() {
        eval(read("(< 2)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn gt_fails_with_less_than_two_args() {
        eval(read("(> 2)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn lte_fails_with_less_than_two_args() {
        eval(read("(<= 2)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn gte_fails_with_less_than_two_args() {
        eval(read("(>= 2)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn equal_works() {
        let (out_expr, _) = eval(read("(= 1 1)".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(true))));
    }
    #[test]
    #[should_fail]
    fn equal_fails_with_less_than_two_params() {
        eval(read("(= 1)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn two_identical_lambdas_are_eq() {
        let (out_expr, _) = eval(read("(= (lambda (x) x) (lambda (x) x))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(true))));
    }
    #[test]
    fn two_lambdas_with_different_arg_lists_and_matching_bodies_arent_equal() {
        let (out_expr, _) = eval(read("(= (lambda (y) 1) (lambda (x) 1))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(false))));
    }
    #[test]
    fn two_lambdas_with_matching_args_lists_and_different_bodies_arent_equal() {
        let (out_expr, _) = eval(read("(= (lambda (y) 1) (lambda (y) 2))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Boolean(false))));
    }
    #[test]
    fn getting_the_length_of_a_three_element_list_should_return_three() {
        let (out_expr, _) = eval(read("(length (quote (1 2 3)))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Number::integer(3))));
    }
    #[test]
    fn getting_the_length_of_an_empty_list_should_return_zero() {
        let (out_expr, _) = eval(read("(length (quote ()))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr, Some(Atom(Number::integer(0))));
    }
    #[test]
    #[should_fail]
    fn getting_the_length_of_an_atom_value_should_fail() {
        eval(read("(length 1)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn cons_should_combine_two_atoms_into_a_new_list() {
        let (out_expr, _) = eval(read("(cons 1 2)".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "(1 2)".to_owned());
    }
    #[test]
    fn cons_should_combine_an_atom_and_a_list_into_a_new_list() {
        let (out_expr, _) = eval(read("(cons 1 (quote (2 3)))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "(1 2 3)".to_owned());
    }
    #[test]
    fn cons_should_combone_a_list_car_and_a_nested_list_cdr_correctly() {
        let (out_expr, _) = eval(read("(cons (quote (1 2 3)) (quote ((4 5 6))))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "((1 2 3) (4 5 6))".to_owned());
    }
    #[test]
    #[should_fail]
    fn cons_should_fail_with_one_arg() {
        eval(read("(cons 1)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn cons_should_fail_with_more_than_two_args() {
        eval(read("(cons 1 2 3)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn car_should_pull_the_first_element_from_a_list() {
        let (out_expr, _) = eval(read("(car (quote (1 2 3)))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "1".to_owned());
    }
    #[test]
    #[should_fail]
    fn car_should_fail_when_applied_to_an_atom_value() {
        eval(read("(car 1)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn cdr_should_pull_the_remaining_elements_from_a_list() {
        let (out_expr, _) = eval(read("(cdr (quote (1 2 3)))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "(2 3)".to_owned());
    }
    #[test]
    fn cdr_should_return_an_empty_list_when_applied_to_a_single_element_list() {
        let (out_expr, _) = eval(read("(cdr (quote (1)))".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "()".to_owned());
    }
    #[test]
    #[should_fail]
    fn cdr_should_fail_when_applied_to_an_atom_value() {
        eval(read("(cdr 1)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn append_adds_an_expr_to_the_end_of_a_list() {
        let (out_expr, _) = eval(read("(append (quote (1)) 2)".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "(1 2)".to_owned());
    }
    #[test]
    #[should_fail]
    fn append_should_fail_if_the_first_arg_isnt_a_list() {
        eval(read("(append 1 2".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn append_should_fail_when_passed_one_arg() {
        eval(read("(append (quote (1)))".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    #[should_fail]
    fn append_should_fail_with_more_than_two_args() {
        eval(read("(append (quote (1)) 1 2)".to_owned()),
             add_builtins(Env::new(None, None, None)));
    }
    #[test]
    fn applying_list_with_zero_args_should_return_an_empty_list() {
        let (out_expr, _) = eval(read("(list)".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().is_null(), true);
    }
    #[test]
    fn applying_list_to_any_number_of_arguments_returns_a_list_of_those_arguments() {
        let (out_expr, env) = eval(read("(list 1 2 3)".to_owned()),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap().print(), "(1 2 3)".to_owned());
        let (out_expr, _) = eval(read("(list 1 (list 2) 3)".to_owned()), env);
        assert_eq!(out_expr.unwrap().print(), "(1 (2) 3)".to_owned());
    }
    #[test]
    fn list_predicate_returns_false_for_atom_values() {
        let (out_expr, _) = eval(read("(list? 1)".to_owned()),
                                 add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn list_predicate_returns_true_for_any_list_value() {
        let (out_expr, env) = eval(read("(list? (list))".to_owned()),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, _) = eval(read("(list? (list 1 2 3))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
    }
    #[test]
    fn null_predicate_returns_true_only_for_empty_list_values() {
        let (out_expr, env) = eval(read("(null? (list))".to_owned()),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, env) = eval(read("(null? (quote x))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, _) = eval(read("(null? (list 1 2 3))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }
    #[test]
    fn symbol_predicate_returns_true_only_for_empty_list_values() {
        let (out_expr, env) = eval(read("(symbol? (list))".to_owned()),
                                   add_builtins(Env::new(None, None, None)));
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
        let (out_expr, env) = eval(read("(symbol? (quote x))".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(true)));
        let (out_expr, _) = eval(read("(symbol? 1)".to_owned()), env);
        assert_eq!(out_expr.unwrap(), Atom(Boolean(false)));
    }

    #[test]
    fn norvig_suite_1() {
        use std::num::strconv;
        use to_str = std::num::strconv::float_to_str_common;
        use std::from_str::FromStr;
        let one = "(define area (lambda (r) (* 3.141592653 (* r r))))".to_owned();
        let (_, env) = eval(read(one),
                            add_builtins(Env::new(None, None, None)));
        let two = "(area 3)".to_owned();
        let (out_expr, env) = eval(read(two), env);
        //assert_eq!(out_expr.unwrap().print().contains("28.27433"), true);
        assert_eq!(out_expr.unwrap().print(), "28.274333877".to_owned());
        let three = "\
            (define fact2                         \
                (lambda (n acc)                   \
                    (if (<= n 1)                  \
                        acc                       \
                        (fact2 (- n 1) (* n acc)) \
                    )                             \
                )                                 \
            )".to_owned();
        let (_, env) = eval(read(three), env);
        let three_point_five = "(define fact (lambda (n) (fact2 n 1)))".to_owned();
        let (_, env) = eval(read(three_point_five), env);
        let four = "(fact 10)".to_owned();
        let (out_expr, env) = eval(read(four), env);
        assert_eq!(out_expr.unwrap().print(), "3628800".to_owned());
        let five = "(fact 100)".to_owned();
        let (out_expr, env) = eval(read(five), env);
        assert_eq!(out_expr.unwrap().print(), "9332621544394415268169923885626".to_owned()+
                   "67004907159682643816214685929638952175999932299156089414639"+
                   "76156518286253697920827223758251185210916864000000000000000"+
                   "000000000");
        let six = "(area (fact 10))".to_owned();
        let (out_expr, env) = eval(read(six), env);
        let fl_val: f64 = FromStr::from_str(out_expr.unwrap().print())
            .expect("should be able to resolve value from printed bigrat");
        let (out_val, _) = to_str(fl_val, 10u, true,
                          strconv::SignNone, strconv::DigMax(10), strconv::ExpDec, false);
        assert_eq!(out_val,
                   "4.1369087198e13".to_owned());
        let seven = "(define first car)".to_owned();
        let (_, env) = eval(read(seven), env);
        let seven = "(define rest cdr)".to_owned();
        let (_, env) = eval(read(seven), env);
        let eight =
            "(define count \
                (lambda (item L acc) \
                    (begin
                        (display (list item L acc)) \
                        (if (> 1 (length L)) \
                            acc \
                            (count item (rest L) (if (equal? item (first L)) \
                                                     (+ acc 1) \
                                                     acc))) \
                    ) \
                ) \
            )".to_owned();
        let (_, env) = eval(read(eight), env);
        let nine = "(count 0 (list 0 1 2 3 0 0) 0)".to_owned();
        let (out_expr, env) = eval(read(nine), env);
        assert_eq!(out_expr.unwrap().print(), "3".to_owned());
        let ten = "(count (quote the) (quote (the more the merrier \
                the bigger the better)) 0)".to_owned();
        let (out_expr, _) = eval(read(ten), env);
        assert_eq!(out_expr.unwrap().print(), "4".to_owned());
    }

    #[test]
    fn syntactic_keyword_in_lambda_doesnt_choke() {
        let eight = "(define foo (lambda (x) (begin (display (quote foo)) 2 x)))".to_owned();
        let (_, env) = eval(read(eight),
                            add_builtins(Env::new(None, None, None)));
        let (out_expr, _) = eval(read("(foo (list 1 2 3))".to_owned()), env);
        assert_eq!(out_expr.unwrap().print(), "(1 2 3)".to_owned());
    }
    #[test]
    fn test_length_of_list_in_if() {
        let env = add_builtins(Env::new(None, None, None));
        let nine = "(if (> 1 (length (list 1 2 3))) (quote conseq) (quote alt))".to_owned();
        let (out_expr, _) = eval(read(nine), env);
        assert_eq!(out_expr.unwrap().print(), "alt".to_owned());
    }
}
