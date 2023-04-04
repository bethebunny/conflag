use std::rc::Rc;

use crate::{
    ast::AstNode, binop::BinOp, builtins::BuiltinFn, scope::ScopePtr, thunk::Thunk, Error,
};

#[derive(Debug, Clone)]
pub enum Value {
    Object(ScopePtr),
    Patch(Thunk),
    Array(Vec<Thunk>),
    Number(f64),
    String(String),
    Boolean(bool),
    FunctionCall {
        f: Thunk,
        args: Vec<Thunk>,
    },
    BuiltinFn(BuiltinFn),
    Lambda {
        scope: ScopePtr,
        arg_names: Vec<String>,
        expr: Rc<AstNode>,
    },
    BinOp {
        kind: BinOp,
        left: Thunk,
        right: Thunk,
    },
    Name(ScopePtr, String),
    Attribute {
        value: Thunk,
        attr: String,
    },
    Null,
}

impl Value {
    pub fn is_primitive(&self) -> bool {
        match self {
            Value::Object(..)
            | Value::Array(..)
            | Value::Patch(..)
            | Value::Number(..)
            | Value::String(..)
            | Value::Boolean(..)
            | Value::Null
            | Value::Lambda { .. }
            | Value::BuiltinFn(..) => true,
            _ => false,
        }
    }

    pub fn at(&self, index: usize) -> Result<Rc<Value>, Error> {
        match &*self {
            Value::Array(values) => values[index].evaluate(),
            _ => Err(Error::BadFunctionCall), // TODO: better error type
        }
    }

    pub fn attr(&self, attr: &str) -> Result<Rc<Value>, Error> {
        match &*self {
            Value::Object(scope) => match scope.get(&attr.into()) {
                Some(thunk) => thunk.evaluate(),
                None => Err(Error::NoSuchAttribute(scope.clone(), attr.into())),
            },
            _ => Err(Error::BadFunctionCall), // TODO: better error type
        }
    }
}
