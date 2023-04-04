#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use conflag;
use conflag::Value;

#[test]
fn test_subtract() {
    let v = conflag::parse("0 - 1").unwrap();
    assert_matches!(*v, Value::Number(n) if n == -1.);
}
