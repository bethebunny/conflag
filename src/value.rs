use core::fmt;
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
        match self {
            Value::Array(values) => values[index].evaluate(),
            _ => Err(Error::BadFunctionCall), // TODO: better error type
        }
    }

    pub fn attr(&self, attr: &str) -> Result<Rc<Value>, Error> {
        match self {
            Value::Object(scope) => match scope.get(&attr.into()) {
                Some(thunk) => thunk.evaluate(),
                None => Err(Error::NoSuchAttribute(scope.clone(), attr.into())),
            },
            _ => Err(Error::BadFunctionCall), // TODO: better error type
        }
    }

    pub fn str(&self) -> &str {
        match self {
            Value::String(s) => s.as_str(),
            _ => panic!("Tried to convert non-string value to str: {:?}", self),
        }
    }

    pub fn number(&self) -> f64 {
        match self {
            Value::Number(s) => *s,
            _ => panic!("Tried to convert non-number value to number: {:?}", self),
        }
    }

    pub fn bool(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            _ => panic!("Tried to convert non-bool value to bool: {:?}", self),
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    // TODO: wrapping for terminal width
    pub fn pretty_indented(
        &self,
        indent: usize,
        _color: bool,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let max_width = f.width().unwrap_or(80);
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Lambda { .. } => write!(f, "<lambda>"),
            Value::BuiltinFn(BuiltinFn(name, ..)) => write!(f, "<builtin: {name}>"),
            Value::Patch(thunk) => {
                write!(f, "&")?;
                match thunk.evaluate() {
                    Ok(v) => write!(f, "{}", v),
                    Err(e) => write!(f, "Error<{e:?}>"),
                }
            }
            Value::Object(scope) => {
                let values = scope.values();
                let mut keys = values.keys().collect::<Vec<_>>();
                keys.sort();
                let pair_strs = keys
                    .iter()
                    .map(|k| {
                        // TODO: what about keys with spaces?
                        format!(
                            "{k}: {}",
                            match scope.get(k).unwrap().evaluate() {
                                Ok(v) => format!("{}", v),
                                Err(e) => format!("Error<{e:?}>"),
                            }
                        )
                    })
                    .collect::<Vec<_>>();
                let total_length: usize = pair_strs.iter().map(|s| s.len()).sum();
                let wraps = pair_strs.iter().any(|s| s.contains("\n"));
                if !wraps && total_length + indent + 2 * (pair_strs.len() + 1) < max_width {
                    write!(f, "{{{}}}", pair_strs.join(", "))?;
                } else {
                    let indent = " ".repeat(indent);
                    write!(f, "{{")?;
                    for pair in pair_strs {
                        if pair.contains("\n") {
                            for line in pair.split("\n") {
                                write!(f, "\n{indent}  {line}")?;
                            }
                        } else {
                            write!(f, "\n{indent}  {pair}")?;
                        }
                    }
                    write!(f, "\n{indent}}}")?;
                }
                Ok(())
            }
            Value::Array(values) => {
                let strs = values
                    .iter()
                    .map(|thunk| match thunk.evaluate() {
                        Ok(v) => format!("{}", v),
                        Err(e) => format!("Error<{e:?}>"),
                    })
                    .collect::<Vec<_>>();
                let total_length: usize = strs.iter().map(|s| s.len()).sum();
                let wraps = strs.iter().any(|s| s.contains("\n"));
                if !wraps && total_length + indent + 2 * (strs.len() + 1) < max_width {
                    write!(f, "[{}]", strs.join(", "))?;
                } else {
                    let indent = " ".repeat(indent);
                    write!(f, "[")?;
                    for str in strs {
                        if str.contains("\n") {
                            for line in str.split("\n") {
                                write!(f, "\n{indent}  {line}")?;
                            }
                        } else {
                            write!(f, "\n{indent}  {str}")?;
                        }
                    }
                    write!(f, "\n{indent}]")?;
                }
                Ok(())
            }
            _ => write!(f, "<pending: {:?}>", self),
        }
    }

    pub fn pretty(&self, color: bool, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_indented(0, color, f)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty(false, f)
    }
}
