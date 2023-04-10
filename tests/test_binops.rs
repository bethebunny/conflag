use conflag;

mod add {
    mod number {
        use super::super::*;

        #[test]
        fn test_add() {
            let v = conflag::parse("0 + 1").unwrap();
            assert!(v.number() == 1.);
        }

        #[test]
        fn test_patch() {
            let v = conflag::parse("1 + &(v) => v + 2").unwrap();
            assert!(v.number() == 3.);
        }
    }

    mod object {
        use super::super::*;

        #[test]
        fn test_left_empty() {
            let v = conflag::parse("{} + {a: 1}").unwrap();
            let expected = conflag::parse("{a: 1}").unwrap();
            assert!(v == expected);
        }

        #[test]
        fn test_right_empty() {
            let v = conflag::parse("{a: 1} + {}").unwrap();
            let expected = conflag::parse("{a: 1}").unwrap();
            assert!(v == expected);
        }
    }

    mod patch {
        use super::super::*;
    }
}
