use crate::{scope::ScopePtr, thunk::Thunk, value::Value, Error};

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Patch,
    ObjectReplace,
}

impl BinOp {
    pub fn evaluate(&self, left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
        match self {
            BinOp::Plus => self.evaluate_plus(left, right),
            BinOp::Patch => BinOp::patch(left, right),
            BinOp::ObjectReplace => BinOp::replace(left, right),
        }
    }

    fn evaluate_plus(&self, left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
        let lv = left.evaluate()?;
        let rv = right.evaluate()?;
        let value = match (&*lv, &*rv) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (Value::String(l), Value::String(r)) => Value::String(l.to_owned() + &r),
            (Value::Array(l), Value::Array(r)) => Value::Array({
                let mut a = l.clone();
                a.append(&mut r.clone());
                a
            }),
            (_, Value::Patch(p)) => Value::BinOp {
                kind: BinOp::Patch,
                left: left.clone(),
                right: p.clone(),
            },
            (Value::Object(s1), Value::Object(s2)) => {
                // TODO: what parent to keep here now?
                // I think the answer is that objects being scopes is outdated.
                let mut values = s1.borrow().values.clone();
                for (k, v) in s2.borrow().values.iter() {
                    let entry = values.entry(k.clone());
                    entry
                        .and_modify(|lv| {
                            *lv = Value::BinOp {
                                kind: BinOp::ObjectReplace,
                                left: lv.clone(),
                                right: v.clone(),
                            }
                            .into();
                        })
                        .or_insert(v.clone());
                }
                Value::Object(ScopePtr::from_values(values, s1.borrow().parent.clone()))
            }
            _ => Err(Error::UnsupportedOperation(*self, lv.clone(), rv.clone()))?,
        };
        Ok(value.into())
    }

    fn patch(left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
        let lv = left.evaluate()?;
        let rv = right.evaluate()?;
        Ok(match (&*lv, &*rv) {
            (_, Value::Lambda { .. } | Value::BuiltinFn(..)) => Value::FunctionCall {
                f: right.clone(),
                args: vec![left.clone()],
            },
            (_, Value::Object(..)) => Value::BinOp {
                kind: BinOp::Plus,
                left: left.clone(),
                right: right.clone(),
            },
            (Value::Patch(l), _) => Value::Patch(
                Value::BinOp {
                    kind: BinOp::Patch,
                    left: l.clone(),
                    right: right.clone().into(),
                }
                .into(),
            ),
            _ => Err(Error::UnsupportedOperation(
                BinOp::Patch,
                lv.clone(),
                rv.clone(),
            ))?,
        }
        .into())
    }

    fn replace(left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
        Ok(match &*right.evaluate()? {
            Value::Patch(p) => Value::BinOp {
                kind: BinOp::Patch,
                left: left.clone(),
                right: p.clone(),
            }
            .into(),
            _ => right.clone(),
        })
    }
}
