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
// - builtins: import, reduce, equal
// - multiplication
// - array splats
// - dict splats
// - derive(FromConfig)
// - list thunks
// - list indexing
// - self arg in functions
// - arrays as linked lists
//   - List: (Thunk, Option<List>)
//   - allow infinite streams

#[derive(Debug, Clone)]
pub enum Error {
    ParseError(pest::error::Error<Rule>),
    NameResolutionError(ScopePtr, String),
    AttributeAccessOnBadType(Rc<Value>, String),
    NoSuchAttribute(ScopePtr, String),
    BadFunctionCall,
    UnsupportedOperation(BinOp, Rc<Value>, Rc<Value>),
    BadEvaluationAccess,
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(parse_error: pest::error::Error<Rule>) -> Self {
        Error::ParseError(parse_error)
    }
}

pub fn parse(contents: &str) -> Result<Rc<Value>, Error> {
    let ast = AstNode::parse(contents)?;
    Thunk::from(ast.value(&builtins::builtins())).evaluate()
}
