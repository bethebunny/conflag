use core::fmt;
use std::{collections::HashMap, rc::Rc};

use crate::thunk::Thunk;

#[derive(Clone)]
pub struct ScopePtr(Rc<Scope>);

#[derive(Debug, Clone)]
pub struct Scope {
    pub values: HashMap<String, Thunk>,
    pub parent: Option<ScopePtr>,
}

impl fmt::Debug for ScopePtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScopePtr({:?})", self.0.values.keys())
    }
}

impl ScopePtr {
    pub(crate) fn new(parent: Option<ScopePtr>) -> Self {
        Self::from_values(HashMap::new(), parent)
    }

    pub(crate) fn from_values(values: HashMap<String, Thunk>, parent: Option<ScopePtr>) -> Self {
        ScopePtr(Rc::new(Scope { values, parent }))
    }

    pub(crate) fn sub_scope(&self, values: HashMap<String, Thunk>) -> ScopePtr {
        ScopePtr::from_values(values, Some(self.clone()))
    }

    pub(crate) fn values(&self) -> &HashMap<String, Thunk> {
        &self.0.values
    }

    pub(crate) fn parent(&self) -> &Option<ScopePtr> {
        &self.0.parent
    }

    // Safety: There's exactly one usage of this function, in AstNode.
    // Since we know that values can only be set in one place, ie. when the AST
    // is being created for the scope, we know we can't have multiple concurrent writes.
    // No other usages of this function are safe.
    pub(crate) unsafe fn set_values(&mut self, values: HashMap<String, Thunk>) {
        Rc::get_mut_unchecked(&mut self.0).values = values;
    }

    pub fn get(&self, name: &String) -> Option<Thunk> {
        self.0.values.get(name).cloned()
    }

    pub fn name_lookup(&self, name: &String) -> Option<Thunk> {
        match (self.get(name), &self.0.parent) {
            (Some(v), _) => Some(v),
            (None, Some(parent)) => parent.name_lookup(name),
            _ => None,
        }
    }
}
