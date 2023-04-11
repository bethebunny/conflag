use core::fmt;
use std::rc::Rc;

use crate::{
    ast::AstNode,
    binop::{BinOp, Comparison},
    builtins::BuiltinFn,
    scope::ScopePtr,
    thunk::Thunk,
    Error, Result,
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

    pub fn at(&self, index: usize) -> Result<Rc<Value>> {
        match self {
            Value::Array(values) => values[index].evaluate(),
            _ => Err(Error::BadFunctionCall), // TODO: better error type
        }
    }

    pub fn attr(&self, attr: &str) -> Result<Rc<Value>> {
        match self {
            Value::Object(scope) => match scope.get(&attr.into()) {
                Some(thunk) => thunk.evaluate(),
                None => Err(Error::NoSuchAttribute(scope.clone(), attr.into())),
            },
            _ => Err(Error::BadFunctionCall), // TODO: better error type
        }
    }

    pub fn str(&self) -> Result<&str> {
        match self {
            Value::String(s) => Ok(s.as_str()),
            _ => Err(Error::Custom(format!(
                "Tried to convert non-string value to str: {:?}",
                self
            ))),
        }
    }

    pub fn number(&self) -> Result<f64> {
        match self {
            Value::Number(s) => Ok(*s),
            _ => Err(Error::Custom(format!(
                "Tried to convert non-number value to number: {:?}",
                self
            ))),
        }
    }

    pub fn bool(&self) -> Result<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            _ => Err(Error::Custom(format!(
                "Tried to convert non-bool value to bool: {:?}",
                self
            ))),
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    fn pretty_format_items(
        &self,
        items: Vec<String>,
        indent: usize,
        max_width: usize,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let total_length: usize = items.iter().map(|s| s.len()).sum();
        let wraps = items.iter().any(|s| s.contains("\n"));
        // `indent` is a proxy for the start of the current line (TODO)
        // each value takes up its own length + 2 (for comma and space separation)
        // and then 1 for the final closing character.
        // Length calculations don't yet consider
        // - terminal control codes (eg. colors)
        // - unicode characters
        let single_line_length = indent + total_length + 2 * items.len() + 1;
        if !wraps && single_line_length < max_width {
            write!(f, "{}", items.join(", "))?;
        } else {
            let indent = " ".repeat(indent);
            for item in items {
                if item.contains("\n") {
                    for line in item.split("\n") {
                        write!(f, "\n{indent}  {line}")?;
                    }
                } else {
                    write!(f, "\n{indent}  {item}")?;
                }
                write!(f, ",")?;
            }
            write!(f, "\n{indent}")?;
        }
        Ok(())
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
                        format!("{k}: {}", scope.get(k).unwrap())
                    })
                    .collect::<Vec<_>>();
                write!(f, "{{")?;
                self.pretty_format_items(pair_strs, indent, max_width, f)?;
                write!(f, "}}")
            }
            Value::Array(values) => {
                let strs = values
                    .iter()
                    .map(|thunk| format!("{thunk}"))
                    .collect::<Vec<_>>();
                write!(f, "[")?;
                self.pretty_format_items(strs, indent, max_width, f)?;
                write!(f, "]")
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

impl std::cmp::PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        Comparison::Equal
            ._compare_native(self, other)
            .unwrap_or(false)
    }
}
