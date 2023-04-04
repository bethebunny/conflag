use std::rc::Rc;

use crate::{scope::ScopePtr, thunk::Thunk, value::Value, Error};

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
    pub fn evaluate(&self, left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
        match self {
            BinOp::Plus => self.evaluate_plus(left, right),
            BinOp::Patch => BinOp::patch(left, right),
            BinOp::ObjectReplace => BinOp::replace(left, right),
            BinOp::Compare(kind) => kind.compare(left, right),
            _ => todo!(),
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

impl Comparison {
    pub fn compare(&self, left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
        let lv = left.evaluate()?;
        let rv = right.evaluate()?;
        Ok(Value::Boolean(self._compare_native(lv, rv)?).into())
    }

    pub fn _compare_native(self, left: Rc<Value>, right: Rc<Value>) -> Result<bool, Error> {
        // println!("Comparing ({:?}):\n\t{:?}\n\t{:?}", self, left, right);
        Ok(match (&*left, &*right) {
            (Value::Object(l), Value::Object(r)) => self.compare_objects(l, r),
            (Value::Array(l), Value::Array(r)) => self.compare_arrays(l, r),
            (Value::Number(l), Value::Number(r)) => self.compare_numbers(l, r),
            (Value::String(l), Value::String(r)) => self.compare_strings(l, r),
            (Value::Boolean(l), Value::Boolean(r)) => self.compare_bools(l, r),
            (Value::Null, Value::Null) => self.compare_nulls(),
            (_, _) => self.compare_different_types(left, right),
        }?)
    }

    fn compare_different_types(&self, _left: Rc<Value>, _right: Rc<Value>) -> Result<bool, Error> {
        Ok(match self {
            Comparison::Equal => false,
            Comparison::NotEqual => true,
            _ => Err(Error::BadFunctionCall)?,
        })
    }

    fn compare_nulls(&self) -> Result<bool, Error> {
        Ok(match self {
            Comparison::Equal => true,
            Comparison::NotEqual => false,
            _ => Err(Error::BadFunctionCall)?,
        })
    }

    fn compare_bools(&self, l: &bool, r: &bool) -> Result<bool, Error> {
        Ok(match self {
            Comparison::Equal => l == r,
            Comparison::NotEqual => l != r,
            _ => Err(Error::BadFunctionCall)?,
        })
    }

    fn compare_strings(&self, l: &String, r: &String) -> Result<bool, Error> {
        Ok(match self {
            Comparison::Equal => l == r,
            Comparison::NotEqual => l != r,
            _ => Err(Error::BadFunctionCall)?,
        })
    }

    fn compare_numbers(&self, l: &f64, r: &f64) -> Result<bool, Error> {
        Ok(match self {
            Comparison::Equal => l == r,
            Comparison::NotEqual => l != r,
            Comparison::GreaterThan => l > r,
            Comparison::GreaterThanOrEqual => l >= r,
            Comparison::LessThan => l < r,
            Comparison::LessThanOrEqual => l <= r,
        })
    }

    fn compare_arrays(&self, l: &Vec<Thunk>, r: &Vec<Thunk>) -> Result<bool, Error> {
        if l.len() != r.len() {
            return Ok(match self {
                Comparison::Equal => false,
                Comparison::NotEqual => true,
                _ => Err(Error::BadFunctionCall)?,
            });
        }
        let mut arrays_equal = true;
        for (left, right) in l.iter().zip(r.iter()) {
            let lv = left.evaluate()?;
            let rv = right.evaluate()?;
            let eq = Comparison::Equal._compare_native(lv, rv)?;
            if !eq {
                arrays_equal = false;
                break;
            }
        }
        Ok(match self {
            Comparison::Equal => arrays_equal,
            Comparison::NotEqual => !arrays_equal,
            _ => Err(Error::BadFunctionCall)?,
        })
    }

    fn compare_objects(&self, l: &ScopePtr, r: &ScopePtr) -> Result<bool, Error> {
        let lvalues = l.values();
        let rvalues = r.values();
        if lvalues.len() != rvalues.len() {
            return Ok(match self {
                Comparison::Equal => false,
                Comparison::NotEqual => true,
                _ => Err(Error::BadFunctionCall)?,
            });
        }
        if !lvalues.keys().eq(rvalues.keys()) {
            return Ok(match self {
                Comparison::Equal => false,
                Comparison::NotEqual => true,
                _ => Err(Error::BadFunctionCall)?,
            });
        }
        let mut objects_equal = true;
        for attr in lvalues.keys() {
            let lv = lvalues[attr].evaluate()?;
            let rv = rvalues[attr].evaluate()?;
            let eq = Comparison::Equal._compare_native(lv, rv)?;
            if !eq {
                objects_equal = false;
                break;
            }
        }
        Ok(match self {
            Comparison::Equal => objects_equal,
            Comparison::NotEqual => !objects_equal,
            _ => Err(Error::BadFunctionCall)?,
        })
    }
}
