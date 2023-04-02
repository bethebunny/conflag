#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use conflag;
use conflag::Value;

#[test]
fn test_call_builtin() {
    let v = conflag::parse("if(true, 1, 2)").unwrap();
    assert_matches!(*v, Value::Number(n) if n == 1.);
}
