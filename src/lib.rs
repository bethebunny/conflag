#![feature(assert_matches)]
#![feature(get_mut_unchecked)]

use std::rc::Rc;

mod ast;
mod binop;
mod builtins;
mod scope;
mod thunk;
mod value;

use ast::{AstNode, Rule};
use binop::BinOp;
pub use builtins::BuiltinFn;
use scope::ScopePtr;
use thunk::Thunk;
pub use value::Value;

#[macro_use]
extern crate pest_derive;

// TODO
// - some actual use cases!
// - docs
// - moar docs
// - lots of tests
// - github
// - CI
// - binops: -, *, /
// - binops: precedence
// - better errors (stop just using BadFunctionCall everywhere lmao)
// - array splats
// - dict splats
// - list thunks
// - list indexing
// - self arg in functions
// - arrays as linked lists
//   - List: (Thunk, Option<List>)
//   - allow infinite streams
// - Sync + Send
// - rethink Rc<Value> as an interface
// - reduce # of extra clones

#[derive(Debug, Clone)]
pub enum Error {
    ParseError(pest::error::Error<Rule>),
    NameResolutionError(ScopePtr, String),
    AttributeAccessOnBadType(Rc<Value>, String),
    NoSuchAttribute(ScopePtr, String),
    BadFunctionCall,
    UnsupportedOperation(BinOp, Rc<Value>, Rc<Value>),
    BadEvaluationAccess,
    Custom(String),
}

type Result<T> = std::result::Result<T, Error>;

impl From<pest::error::Error<Rule>> for Error {
    fn from(parse_error: pest::error::Error<Rule>) -> Self {
        Error::ParseError(parse_error)
    }
}

impl From<&Error> for Error {
    fn from(value: &Error) -> Self {
        value.clone()
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParseError(e) => write!(f, "Parse error: {e}"),
            _ => write!(f, "{:?}", self),
        }
    }
}

pub fn parse(contents: &str) -> Result<Rc<Value>> {
    let ast = AstNode::parse(contents)?;
    Thunk::from(ast.value(&builtins::builtins())).evaluate()
}

#[cfg(feature = "serde")]
extern crate serde as extern_serde;

#[cfg(feature = "serde")]
pub mod serde;

#[cfg(feature = "serde")]
impl extern_serde::de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Error::Custom(format!("{}", msg))
    }
}

#[cfg(feature = "serde")]
impl extern_serde::ser::StdError for Error {}

#[cfg(feature = "python")]
pub mod py;

#[cfg(feature = "python")]
impl From<Error> for pyo3::PyErr {
    fn from(value: Error) -> Self {
        crate::py::ConflagError::new_err(format!("{value}"))
    }
}
