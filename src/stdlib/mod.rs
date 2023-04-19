use std::{collections::HashMap, rc::Rc};

use crate::{thunk::Thunk, BuiltinFn, Value};

mod math;

pub(crate) fn modules() -> HashMap<String, Thunk> {
    let mut modules = native_modules();
    // TODO: automatically pull libraries from lib dir
    modules.insert("core".into(), lazy_module(include_str!("core.cfg")));
    modules
}

fn native_modules() -> HashMap<String, Thunk> {
    ([("math".into(), math::module())]).into()
}

fn lazy_module(content: &'static str) -> Thunk {
    let lazy: Thunk = Value::BuiltinFn(BuiltinFn(
        "__lazy_import".into(),
        Rc::new(move |_| {
            // TODO: this needs an import context from somewhere :P
            Ok(crate::parse(content).unwrap().into())
        }),
    ))
    .into();
    Value::FunctionCall {
        f: lazy,
        args: vec![],
    }
    .into()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_modules() {
        let modules = modules();
        let math = modules["math"].evaluate().unwrap();
        assert!(math.attr("sqrt").is_ok());
    }
}
