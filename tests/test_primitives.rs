#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use conflag;
use conflag::Value;

#[test]
fn test_number() {
    let v = conflag::parse("4").unwrap();
    assert_matches!(*v, Value::Number(n) if n == 4.);
    let v = conflag::parse("0").unwrap();
    assert_matches!(*v, Value::Number(n) if n == 0.);
    let v = conflag::parse("0.4").unwrap();
    assert_matches!(*v, Value::Number(n) if n == 0.4);
}

#[test]
fn test_bool() {
    let v_true = conflag::parse("true").unwrap();
    assert_matches!(*v_true, Value::Boolean(true));
    let v_false = conflag::parse("false").unwrap();
    assert_matches!(*v_false, Value::Boolean(false));
}

#[test]
fn test_null() {
    let v = conflag::parse("null").unwrap();
    assert_matches!(*v, Value::Null);
}

// trait FromConflag: From<conflag::Value> {}

// #[derive(FromConflag, Debug)]
// struct Record {
//     name: String,
//     age: i32,
// }

// #[proc_macro_derive(FromConflag)]
// fn derive(input: TokenStream) -> TokenStream {}

// #[test]
// fn test_record_into() {
//     let v = conflag::parse(
//         r#"{
//         name: "Stef",
//         age: 33,
//         ssn: "XXX-XX-XXXX",
//     }"#,
//     )
//     .unwrap();
//     let record: Record = v.into();
//     assert_matches!(
//         record,
//         Record {
//             name,
//             age: 33
//         } if name == "stef"
//     );
// }
