use core::fmt;
use std::{collections::HashMap, fs};

use crate::{ast::AstNode, scope::ScopePtr, thunk::Thunk, value::Value, Error};

type _BuiltinFn = fn(&Vec<Thunk>) -> Result<Thunk, Error>;

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

fn builtin_bool(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if args.len() != 1 {
        Err(Error::BadFunctionCall)?;
    }
    let v = args.first().unwrap().evaluate();
    Ok(Value::Boolean(match &*v.clone()? {
        Value::Object(scope) => scope.values().len() != 0,
        Value::Array(values) => values.len() != 0,
        Value::Number(num) => *num != 0.,
        Value::Boolean(val) => *val,
        Value::String(val) => val != "",
        Value::Null => false,
        Value::Lambda { .. } => true,
        _ => Err(Error::BadFunctionCall)?,
    })
    .into())
}

fn builtin_if(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if let [pred, true_value, false_value] = &args[..] {
        let bool_args = vec![pred.clone()];
        let pred = builtin_bool(&bool_args)?;
        Ok(match &*pred.evaluate()? {
            Value::Boolean(true) => true_value.clone(),
            Value::Boolean(false) => false_value.clone(),
            _ => unreachable!(),
        })
    } else {
        Err(Error::BadFunctionCall)
    }
}

fn builtin_map(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if let [f, array] = &args[..] {
        match &*array.evaluate()? {
            // TODO: Hmm I want to return an un-evaluated array here; will that be a problem?
            //       _I think_ my execution model makes the assumption that .evaluate() is "final" right now.
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
            _ => Err(Error::BadFunctionCall),
        }
    } else {
        Err(Error::BadFunctionCall)
    }
}

fn builtin_import(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if let [target] = &args[..] {
        match &*target.evaluate()? {
            Value::String(path) => {
                let contents = fs::read_to_string(path).or_else(|_| Err(Error::BadFunctionCall))?;
                let node = AstNode::parse(contents.as_str())?;
                Ok(node.value(&builtins()).into())
            }
            _ => Err(Error::BadFunctionCall),
        }
    } else {
        Err(Error::BadFunctionCall)
    }
}

fn builtin_reduce(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if let [f, array, state] = &args[..] {
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
            _ => Err(Error::BadFunctionCall),
        }
    } else {
        Err(Error::BadFunctionCall)
    }
}

pub(crate) fn builtins() -> ScopePtr {
    let builtins: [(&str, _BuiltinFn); 5] = [
        ("if", builtin_if),
        ("bool", builtin_bool),
        ("map", builtin_map),
        ("import", builtin_import),
        ("reduce", builtin_reduce),
    ];
    let values =
        HashMap::from(builtins.map(|(name, f)| (name.into(), BuiltinFn(name.into(), f).into())));
    ScopePtr::from_values(values, None)
}
