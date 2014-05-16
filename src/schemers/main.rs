// Copyright 2013-2014 Jeffery Olson
//
// Licensed under the 3-Clause BSD License, see LICENSE.txt
// at the top-level of this repository.
// This file may not be copied, modified, or distributed
// except according to those terms.
use std::os::args;

fn run(_input: ~str) -> ~str {
    ~""
}

fn main() {
    let _args = args();
    println!("{}", run(~""));
}

#[cfg(test)]
mod test {
    use super::run;
    
    #[test]
    fn number_literal_evals_to_itself() {
        assert!(run(~"12") == ~"12");
    }
}
