use core::fmt;
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{builtins::BuiltinFn, value::Value, Error, Result};

type ThunkMemo = RefCell<Option<Result<Rc<Value>>>>;

#[derive(Clone)]
pub struct ThunkBody {
    value: Rc<Value>,
    evaluated: Rc<ThunkMemo>,
}

#[derive(Clone)]
pub struct Thunk(Rc<RefCell<ThunkBody>>);

impl fmt::Debug for Thunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.evaluated().borrow() {
            Some(evaluated) => write!(f, "Thunk<done>({:?})", evaluated),
            None => write!(f, "Thunk<pending>({:?})", self.value()),
        }
    }
}

impl Thunk {
    pub fn evaluate(&self) -> Result<Rc<Value>> {
        if !self.is_evaluated() {
            self.set_evaluated(self.try_evaluate());
        }
        self.evaluated().borrow().clone().unwrap()
    }

    #[inline]
    fn try_evaluate(&self) -> Result<Rc<Value>> {
        // Tail optimization
        // - The starting thunk's evaluation pointer is re-used for all intermediate thunks returned
        //   as tail optimized calls.
        // - Intermediate non-tail optimized calls will recursively call `evaluate`, adding to the stack depth.
        // - When we get to a final primitive result (or an error), copy a pointer to its value into the
        //   shared evaluation pointer.
        // - Because we're using RCs, we won't do any non-trivial data cloning or copying for any of these operations.
        // - Max stack depth appears relatively limited without further optimization, but more than enough
        //   for any realistic configuration (O(10k) deep stack calls in release builds with ulimit 8mb stack size).
        let result = &self.0.borrow().evaluated;
        let mut thunk = self.iter_eval()?;

        while !thunk.is_evaluated() {
            thunk.0.borrow_mut().evaluated = result.clone();
            thunk = thunk.iter_eval()?;
        }

        if !Rc::ptr_eq(&self.0, &thunk.0) {
            *result.borrow_mut() = thunk.evaluated().borrow().clone();
        }

        self.evaluated().borrow().clone().unwrap()
    }

    fn is_evaluated(&self) -> bool {
        self.evaluated().borrow().is_some() || {
            let value = self.value();
            if value.is_primitive() {
                self.set_evaluated(Ok(value.clone()));
                true
            } else {
                false
            }
        }
    }

    pub(crate) fn evaluated(&self) -> Ref<ThunkMemo> {
        Ref::map(self.0.borrow(), |v| &*v.evaluated)
    }

    fn set_evaluated(&self, result: Result<Rc<Value>>) {
        *self.0.borrow().evaluated.borrow_mut() = Some(result)
    }

    fn value(&self) -> Ref<Rc<Value>> {
        Ref::map(self.0.borrow(), |body| &body.value)
    }

    #[inline]
    fn iter_eval(&self) -> Result<Thunk> {
        match &**self.value() {
            Value::FunctionCall { f, args } => match &*f.evaluate()? {
                Value::Lambda {
                    scope,
                    arg_names,
                    expr,
                } => {
                    let scope = scope.sub_scope(
                        arg_names
                            .iter()
                            .cloned()
                            .zip(args.iter().cloned())
                            .collect(),
                    );
                    Ok(expr.value(&scope).into())
                }
                Value::BuiltinFn(BuiltinFn(_, f)) => f(args),
                _ => Err(Error::BadFunctionCall),
            },
            Value::Name(scope, name) => scope
                .name_lookup(name)
                .ok_or_else(|| Error::NameResolutionError(scope.clone(), name.clone())),
            Value::Attribute { value, attr } => match &*value.evaluate()? {
                Value::Object(scope) => scope
                    .get(attr)
                    .ok_or_else(|| Error::NoSuchAttribute(scope.clone(), attr.clone())),
                v => Err(Error::AttributeAccessOnBadType(
                    Rc::new(v.clone()),
                    attr.clone(),
                )),
            },
            Value::BinOp { kind, left, right } => kind.evaluate(left, right),
            _ => unreachable!("Should never ask for dependencies for: {:?}", self),
        }
    }
}

impl From<Value> for Thunk {
    fn from(v: Value) -> Self {
        Rc::new(v).into()
    }
}

impl From<Rc<Value>> for Thunk {
    fn from(v: Rc<Value>) -> Self {
        Thunk(Rc::new(RefCell::new(ThunkBody {
            value: v,
            evaluated: Rc::new(RefCell::new(None)),
        })))
    }
}

impl fmt::Display for Thunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.evaluate() {
            Ok(v) => v.fmt(f),
            Err(e) => write!(f, "Error<{e:?}>"),
        }
    }
}
