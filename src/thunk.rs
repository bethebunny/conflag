use core::fmt;
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use crate::{builtins::BuiltinFn, value::Value, Error};

#[derive(Clone)]
pub struct ThunkBody {
    value: Rc<Value>,
    evaluated: Rc<RefCell<Option<Result<Rc<Value>, Error>>>>,
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
    pub fn evaluate(&self) -> Result<Rc<Value>, Error> {
        self.evaluate_tail_optimized()?;
        self.evaluated().borrow().clone().unwrap()
    }

    fn evaluate_tail_optimized(&self) -> Result<(), Error> {
        // Invariants:
        // - Think of the stack as "bottom up", ie. added elements are added to the top
        // - Thunks on the stack can depend only on thunks "higher" on the stack
        // - When evaluating the top thunk on the stack, it may push new thunks to the stack instead
        // let mut stack: Vec<Thunk> = vec![self.clone()];

        let mut thunks = vec![self.clone()];

        while let Some(thunk) = thunks.last() {
            if thunk.is_evaluated() {
                for prev_thunk in &thunks[..thunks.len() - 1] {
                    prev_thunk.tail_resolve(thunk);
                }
                break;
            }
            // println!("EVALUATE: {:?}", thunk);
            thunks.push(thunk.iter_eval()?);
        }
        Ok(())
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

    pub(crate) fn evaluated<'a>(&'a self) -> Ref<'a, RefCell<Option<Result<Rc<Value>, Error>>>> {
        Ref::map(self.0.borrow(), |v| &*v.evaluated)
    }

    fn set_evaluated(&self, result: Result<Rc<Value>, Error>) {
        *self.0.borrow().evaluated.borrow_mut() = Some(result)
    }

    fn value<'a>(&'a self) -> Ref<'a, Rc<Value>> {
        Ref::map(self.0.borrow(), |body| &body.value)
    }

    fn tail_resolve(&self, other: &Thunk) {
        self.0.borrow_mut().evaluated = other.0.borrow().evaluated.clone();
    }

    fn iter_eval(&self) -> Result<Thunk, Error> {
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
                Value::BuiltinFn(BuiltinFn(_, f)) => f(&args),
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
