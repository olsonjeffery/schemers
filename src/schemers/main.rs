// Copyright 2013-2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
use std::os::args;

// this is starting out as a straight port of Peter Norvig's
// lis.py (the first iteration) to Rust

fn main() {
    let _args = args();
    println!("{}", ~"");
}

fn pad_input(input: ~str) -> ~str {
    input.replace("(", " ( ").replace(")", " ) ")
}

#[cfg(test)]
mod test {
    use super::pad_input;
    
    #[test]
    fn pad_input_should_insert_spaces_before_and_after_parens() {
        assert!(pad_input(~"(x 1 2 3)") == ~" ( x 1 2 3 ) ");
    }
}
