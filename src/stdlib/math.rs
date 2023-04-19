use std::collections::HashMap;

use crate::{scope::ScopePtr, thunk::Thunk, BuiltinFn, Error, Result, Value};

pub(crate) fn module() -> Thunk {
    let module_fns = [("sqrt", BuiltinFn::from_1("sqrt", "x", sqrt))];
    let values = HashMap::from(module_fns.map(|(name, f)| (name.into(), f.into())));
    Value::Object(ScopePtr::from_values(values, None)).into()
}

fn sqrt(x: &Thunk) -> Result<Thunk> {
    match &*x.evaluate()? {
        Value::Number(n) => Ok(Value::Number(n.sqrt()).into()),
        _ => Err(Error::TypeError(
            "sqrt(x) expected number".into(),
            x.clone(),
        )),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::{assert_matches::assert_matches, rc::Rc};

    #[test]
    fn test_sqrt() {
        let input = Value::Number(4.);
        let result: Rc<Value> = sqrt(&input.into()).unwrap().evaluate().unwrap();
        assert_eq!(2., result.number().unwrap());
    }

    #[test]
    fn test_sqrt_invalid_type() {
        let input = Value::String("4".into());
        let result = sqrt(&input.into());
        assert_matches!(result, Err(Error::TypeError(..)));
    }

    #[test]
    fn test_sqrt_no_args() {
        let module = module().evaluate().unwrap();
        let value = module.attr("sqrt").unwrap();
        if let Value::BuiltinFn(BuiltinFn(_, sqrt_fn)) = &*value {
            let inputs = vec![];
            let result = sqrt_fn(&inputs);
            assert_matches!(result, Err(Error::BuiltinInvalidArguments(..)));
        } else {
            panic!("didn't get a BuiltinFn")
        }
    }

    #[test]
    fn test_module() {
        let module = module().evaluate().unwrap();
        let value = module.attr("sqrt").unwrap();
        if let Value::BuiltinFn(BuiltinFn(_, sqrt_fn)) = &*value {
            let inputs = vec![Value::Number(4.).into()];
            let result: Rc<Value> = sqrt_fn(&inputs).unwrap().evaluate().unwrap();
            assert_eq!(2., result.number().unwrap());
        } else {
            panic!("didn't get a BuiltinFn")
        }
    }
}
