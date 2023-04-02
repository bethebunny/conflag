#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use conflag;
use conflag::{BuiltinFn, Error, Value};

#[test]
fn test_builtin() {
    let v = conflag::parse("if").unwrap();
    assert_matches!(&*v, Value::BuiltinFn(BuiltinFn(name, ..)) if name == "if");
}

#[test]
fn test_builtin_missing() {
    let v = conflag::parse("__no_such_builtin__");
    assert!(v.is_err());
    assert_matches!(v, Err(Error::NameResolutionError(..)));
}

#[test]
fn test_local() {
    let v = conflag::parse(
        r#"{
        a: 1,
        test: a,
    }.test"#,
    )
    .unwrap();
    assert_matches!(*v, Value::Number(n) if n == 1.);
}

#[test]
fn test_local_shadows_builtin() {
    let v = conflag::parse(
        r#"{
        if: 1,
        test: if,
    }.test"#,
    )
    .unwrap();
    assert_matches!(*v, Value::Number(n) if n == 1.);
}

#[test]
fn test_inner() {
    let v = conflag::parse(
        r#"{
        a: 1,
        test: {
            b: a,
        }.b,
    }.test"#,
    )
    .unwrap();
    assert_matches!(*v, Value::Number(n) if n == 1.);
}

#[test]
fn test_inner_multiple_scopes() {
    let v = conflag::parse(
        r#"{
        a: 1,
        test: {
            b: {
                c: a,
            },
        }.b.c,
    }.test"#,
    )
    .unwrap();
    assert_matches!(*v, Value::Number(n) if n == 1.);
}

#[test]
fn test_inner_shadows_outer() {
    let v = conflag::parse(
        r#"{
        a: 1,
        test: {
            a: 2,
            b: a,
        }.b,
    }.test"#,
    )
    .unwrap();
    assert_matches!(*v, Value::Number(n) if n == 2.);
}
