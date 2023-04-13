use crate::{scope::ScopePtr, thunk::Thunk, value::Value, Error, Result};

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Patch,
    ObjectReplace,
    Subtract,
    Multiply,
    Divide,
    Compare(Comparison),
}

#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Equal,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    NotEqual,
}

impl BinOp {
    pub fn evaluate(&self, left: &Thunk, right: &Thunk) -> Result<Thunk> {
        match self {
            BinOp::Plus => self.evaluate_plus(left, right),
            BinOp::Patch => BinOp::patch(left, right),
            BinOp::ObjectReplace => BinOp::replace(left, right),
            BinOp::Compare(kind) => kind.compare(left, right),
            _ => todo!(),
        }
    }

    fn evaluate_plus(&self, left: &Thunk, right: &Thunk) -> Result<Thunk> {
        let rv = right.evaluate()?;
        Ok(match &*rv {
            Value::Patch(p) => Value::BinOp {
                kind: BinOp::Patch,
                left: left.clone(),
                right: p.clone(),
            },
            r => {
                let lv = left.evaluate()?;
                match (&*lv, r) {
                    (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                    (Value::String(l), Value::String(r)) => Value::String(l.to_owned() + r),
                    (Value::Array(l), Value::Array(r)) => Value::Array({
                        let mut a = l.clone();
                        a.append(&mut r.clone());
                        a
                    }),
                    (Value::Object(s1), Value::Object(s2)) => {
                        // TODO: what parent to keep here now?
                        // I think the answer is that objects being scopes is outdated.
                        let mut values = s1.values().clone();
                        for (k, v) in s2.values().iter() {
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
                        Value::Object(ScopePtr::from_values(values, s1.parent().clone()))
                    }
                    _ => Err(Error::UnsupportedOperation(*self, lv, rv))?,
                }
            }
        }
        .into())
    }

    fn patch(left: &Thunk, right: &Thunk) -> Result<Thunk> {
        let rv = right.evaluate();
        Ok(match &**rv.as_ref()? {
            Value::Lambda { .. } | Value::BuiltinFn(..) => Value::FunctionCall {
                f: right.clone(),
                args: vec![left.clone()],
            },
            Value::Object(..) => Value::BinOp {
                kind: BinOp::Plus,
                left: left.clone(),
                right: right.clone(),
            },
            _ => {
                let lv = left.evaluate();
                if let Value::Patch(l) = &**lv.as_ref()? {
                    Value::BinOp {
                        kind: BinOp::Patch,
                        left: l.clone(),
                        right: right.clone(),
                    }
                } else {
                    Err(Error::UnsupportedOperation(
                        BinOp::Patch,
                        lv.unwrap(),
                        rv.unwrap(),
                    ))?
                }
            }
        }
        .into())
    }

    fn replace(left: &Thunk, right: &Thunk) -> Result<Thunk> {
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

impl Comparison {
    pub fn compare(&self, left: &Thunk, right: &Thunk) -> Result<Thunk> {
        Ok(Value::Boolean(self._compare(left, right)?).into())
    }

    fn _compare(&self, left: &Thunk, right: &Thunk) -> Result<bool> {
        let lv = left.evaluate()?;
        let rv = right.evaluate()?;
        self.compare_values(&lv, &rv)
    }

    pub fn compare_values(&self, left: &Value, right: &Value) -> Result<bool> {
        match (left, right) {
            (Value::Object(l), Value::Object(r)) => self.compare_objects(l, r),
            (Value::Array(l), Value::Array(r)) => self.compare_arrays(l, r),
            (Value::Number(l), Value::Number(r)) => self.compare_numbers(l, r),
            (Value::String(l), Value::String(r)) => self.compare_strings(l, r),
            (Value::Boolean(l), Value::Boolean(r)) => self.compare_bools(l, r),
            (Value::Null, Value::Null) => self.compare_nulls(),
            (_, _) => self.compare_different_types(left, right),
        }
    }

    // TODO: don't require value-level clone for unsupported error
    fn unsupported<K>(&self, left: &Value, right: &Value) -> Result<K> {
        Err(Error::UnsupportedOperation(
            BinOp::Compare(*self),
            left.clone().into(),
            right.clone().into(),
        ))
    }

    fn compare_different_types(&self, left: &Value, right: &Value) -> Result<bool> {
        Ok(match self {
            Comparison::Equal => false,
            Comparison::NotEqual => true,
            _ => self.unsupported(left, right)?,
        })
    }

    fn compare_nulls(&self) -> Result<bool> {
        Ok(match self {
            Comparison::Equal => true,
            Comparison::NotEqual => false,
            _ => self.unsupported(&Value::Null, &Value::Null)?,
        })
    }

    fn compare_bools(&self, l: &bool, r: &bool) -> Result<bool> {
        Ok(match self {
            Comparison::Equal => l == r,
            Comparison::NotEqual => l != r,
            _ => self.unsupported(&Value::Boolean(*l), &Value::Boolean(*r))?,
        })
    }

    fn compare_strings(&self, l: &String, r: &String) -> Result<bool> {
        Ok(match self {
            Comparison::Equal => l == r,
            Comparison::NotEqual => l != r,
            _ => self.unsupported(&Value::String(l.clone()), &Value::String(r.clone()))?,
        })
    }

    fn compare_numbers(&self, l: &f64, r: &f64) -> Result<bool> {
        Ok(match self {
            Comparison::Equal => l == r,
            Comparison::NotEqual => l != r,
            Comparison::GreaterThan => l > r,
            Comparison::GreaterThanOrEqual => l >= r,
            Comparison::LessThan => l < r,
            Comparison::LessThanOrEqual => l <= r,
        })
    }

    fn compare_arrays(&self, l: &Vec<Thunk>, r: &Vec<Thunk>) -> Result<bool> {
        if !matches!(self, Comparison::Equal | Comparison::NotEqual) {
            self.unsupported(&Value::Array(l.clone()), &Value::Array(r.clone()))?;
        }

        let arrays_equal = l.len() == r.len() && {
            let mut equal = true;
            for (left, right) in l.iter().zip(r.iter()) {
                if !Comparison::Equal._compare(left, right)? {
                    equal = false;
                    break;
                }
            }
            equal
        };

        Ok(match self {
            Comparison::Equal => arrays_equal,
            Comparison::NotEqual => !arrays_equal,
            _ => unreachable!(),
        })
    }

    fn compare_objects(&self, l: &ScopePtr, r: &ScopePtr) -> Result<bool> {
        if !matches!(self, Comparison::Equal | Comparison::NotEqual) {
            self.unsupported(&Value::Object(l.clone()), &Value::Object(r.clone()))?;
        }

        let lvalues = l.values();
        let rvalues = r.values();

        let objects_equal =
            lvalues.len() == rvalues.len() && lvalues.keys().eq(rvalues.keys()) && {
                let mut equal = true;
                for attr in lvalues.keys() {
                    if !Comparison::Equal._compare(&lvalues[attr], &rvalues[attr])? {
                        equal = false;
                        break;
                    }
                }
                equal
            };

        Ok(match self {
            Comparison::Equal => objects_equal,
            Comparison::NotEqual => !objects_equal,
            _ => unreachable!(),
        })
    }
}
