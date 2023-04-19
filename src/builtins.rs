use core::fmt;
use std::{collections::HashMap, fs, rc::Rc};

use crate::{ast::AstNode, scope::ScopePtr, thunk::Thunk, value::Value, Error, Result};

// TODO:
// - rewrite builtins using from_N interface
// - signatures / somehow docs in Display
// - unified Display for native fns and lambdas
// - name consistency: builtin vs native fn

type _BuiltinFn = Rc<dyn Fn(&[Thunk]) -> Result<Thunk>>;

#[derive(Clone)]
pub struct BuiltinFn(pub String, pub _BuiltinFn);

impl From<BuiltinFn> for Thunk {
    fn from(value: BuiltinFn) -> Self {
        Value::BuiltinFn(value).into()
    }
}

impl fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "builtin_{:?}", self.0)
    }
}

impl BuiltinFn {
    pub(crate) fn from_1<F>(name: &str, argnames: &str, f: F) -> Self
    where
        F: Fn(&Thunk) -> Result<Thunk> + 'static,
    {
        let signature = format!("{name}({argnames})");
        BuiltinFn(
            name.into(),
            Rc::new(move |args: &[Thunk]| {
                if let [v] = args {
                    f(v)
                } else {
                    builtin_invalid_args(signature.as_str(), args)
                }
            }),
        )
    }
}

fn builtin_invalid_args<K>(name: &str, args: &[Thunk]) -> Result<K> {
    Err(Error::BuiltinInvalidArguments(name.into(), args.to_vec()))
}

fn builtin_bool(args: &[Thunk]) -> Result<Thunk> {
    if let [v] = args {
        Ok(Value::Boolean(match &*v.evaluate()? {
            Value::Object(scope) => !scope.values().is_empty(),
            Value::Array(values) => !values.is_empty(),
            Value::Number(num) => *num != 0.,
            Value::Boolean(val) => *val,
            Value::String(val) => !val.is_empty(),
            Value::Null => false,
            Value::Lambda { .. } => true,
            _ => Err(Error::TypeError("bool not supported".into(), v.clone()))?,
        })
        .into())
    } else {
        builtin_invalid_args("bool(v)", args)
    }
}

fn builtin_if(args: &[Thunk]) -> Result<Thunk> {
    if let [pred, true_value, false_value] = args {
        let bool_args = vec![pred.clone()];
        let pred = builtin_bool(&bool_args)?;
        Ok(match &*pred.evaluate()? {
            Value::Boolean(true) => true_value.clone(),
            Value::Boolean(false) => false_value.clone(),
            _ => unreachable!(),
        })
    } else {
        builtin_invalid_args("if(pred, t, f)", args)
    }
}

fn builtin_map(args: &[Thunk]) -> Result<Thunk> {
    if let [f, array] = args {
        match &*array.evaluate()? {
            Value::Array(values) => {
                let mapped = values.iter().map(|v| {
                    Value::FunctionCall {
                        f: f.clone(),
                        args: vec![v.clone()],
                    }
                    .into()
                });
                Ok(Value::Array(mapped.collect()).into())
            }
            Value::Object(scope) => {
                let mapped = scope
                    .values()
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            Value::FunctionCall {
                                f: f.clone(),
                                args: vec![v.clone()],
                            }
                            .into(),
                        )
                    })
                    .collect();
                Ok(Value::Object(ScopePtr::from_values(mapped, Some(scope.clone()))).into())
            }
            _ => Err(Error::TypeError(
                "map unsupported over".into(),
                array.clone(),
            )),
        }
    } else {
        builtin_invalid_args("map(f, array | object)", args)
    }
}

fn builtin_import(args: &[Thunk]) -> Result<Thunk> {
    if let [target] = args {
        match &*target.evaluate()? {
            Value::String(path) => {
                let lib = crate::stdlib::modules();
                if let Some(module) = lib.get(path) {
                    return Ok(module.clone());
                }
                let contents =
                    fs::read_to_string(path).map_err(|e| Error::ImportReadError(Rc::new(e)))?;
                let node = AstNode::parse(contents.as_str())?;
                Ok(node.value(&builtins()).into())
            }
            _ => Err(Error::TypeError(
                "must import string path".into(),
                target.clone(),
            )),
        }
    } else {
        builtin_invalid_args("import(path)", args)
    }
}

fn builtin_reduce(args: &[Thunk]) -> Result<Thunk> {
    if let [f, array, state] = args {
        match &*array.evaluate()? {
            Value::Array(values) => {
                let mut state = state.clone();
                for v in values.iter() {
                    state = Value::FunctionCall {
                        f: f.clone(),
                        args: vec![state.clone(), v.clone()],
                    }
                    .into();
                }
                Ok(state)
            }
            _ => Err(Error::TypeError(
                "reduce unsupported over".into(),
                array.clone(),
            )),
        }
    } else {
        builtin_invalid_args("reduce(f, array, initial_state)", args)
    }
}

fn builtin_displayed(args: &[Thunk]) -> Result<Thunk> {
    if let [v] = args {
        println!("{}", v);
        Ok(v.clone())
    } else {
        builtin_invalid_args("displayed(value)", args)
    }
}

pub(crate) fn builtins() -> ScopePtr {
    let builtins = [
        ("if", builtin_if as fn(&[Thunk]) -> Result<Thunk>),
        ("bool", builtin_bool),
        ("map", builtin_map),
        ("import", builtin_import),
        ("reduce", builtin_reduce),
        ("displayed", builtin_displayed),
    ];
    let values = HashMap::from(
        builtins.map(|(name, f)| (name.into(), BuiltinFn(name.into(), Rc::new(f)).into())),
    );
    ScopePtr::from_values(values, None)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_import_stdlib() {
        let args = vec![Value::String("math".into()).into()];
        let imported = builtin_import(&args).unwrap().evaluate().unwrap();
        assert!(imported.attr("sqrt").is_ok());
    }
}
