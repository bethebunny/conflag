#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use conflag;
use conflag::Value;

mod add {
    mod number {
        use super::super::*;

        #[test]
        fn test_add() {
            let v = conflag::parse("0 + 1").unwrap();
            assert!(v.number() == 1.);
        }
    }
    mod object {
        use super::super::*;
    }

    mod patch {
        use super::super::*;
    }
}
