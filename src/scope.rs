use core::fmt;
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use crate::thunk::Thunk;

#[derive(Clone)]
pub struct ScopePtr(Rc<RefCell<Scope>>);

#[derive(Debug, Clone)]
pub struct Scope {
    pub values: HashMap<String, Thunk>,
    pub parent: Option<ScopePtr>,
}

impl fmt::Debug for ScopePtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ScopePtr({:?})", self.borrow().values.keys())
    }
}

impl ScopePtr {
    pub(crate) fn new(parent: Option<ScopePtr>) -> Self {
        ScopePtr(Rc::new(RefCell::new(Scope {
            values: HashMap::new(),
            parent,
        })))
    }

    pub(crate) fn from_values(values: HashMap<String, Thunk>, parent: Option<ScopePtr>) -> Self {
        ScopePtr(Rc::new(RefCell::new(Scope { values, parent })))
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, Scope> {
        self.0.borrow()
    }

    pub(crate) fn borrow_mut<'a>(&'a self) -> RefMut<'a, Scope> {
        self.0.borrow_mut()
    }

    pub(crate) fn sub_scope(&self, values: HashMap<String, Thunk>) -> ScopePtr {
        ScopePtr::from_values(values, Some(self.clone()))
    }

    pub fn get(&self, name: &String) -> Option<Thunk> {
        self.borrow().values.get(name).cloned()
    }

    pub fn name_lookup(&self, name: &String) -> Option<Thunk> {
        match (self.get(name), &self.borrow().parent) {
            (Some(v), _) => Some(v),
            (None, Some(parent)) => parent.name_lookup(name),
            _ => None,
        }
    }
}
