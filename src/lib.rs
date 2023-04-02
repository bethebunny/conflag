extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use pest::Parser;

#[derive(Parser)]
#[grammar = "conflag.pest"]
struct ConflagParser;

#[derive(Clone)]
pub struct ScopePtr(Rc<RefCell<Scope>>);

#[derive(Debug, Clone)]
pub struct Scope {
    values: HashMap<String, Thunk>,
    parent: Option<ScopePtr>,
}

// TODO
// - get compiling again
// - comments
// - array splats
// - dict splats
// - list thunks
// - list indexing
// - self arg in functions
// - docs
// - moar docs
// - code organization
// - decide if we can change objects to not be scopes
// - think more carefully about where `evaluate` logic belongs
// - clear pattern for making a thunk out of a value that doesn't need a scope
// - arrays as linked lists
//   - List: (Thunk, Option<List>)
//   - allow infinite streams

impl fmt::Debug for ScopePtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write!(f, "ScopePtr({:p})", self.0)
        write!(f, "ScopePtr({:?})", self.borrow().values.keys())
    }
}

impl ScopePtr {
    fn new(parent: Option<ScopePtr>) -> Self {
        ScopePtr(Rc::new(RefCell::new(Scope {
            values: HashMap::new(),
            parent,
        })))
    }

    fn from_values(values: HashMap<String, Thunk>, parent: Option<ScopePtr>) -> Self {
        ScopePtr(Rc::new(RefCell::new(Scope { values, parent })))
    }

    fn borrow<'a>(&'a self) -> Ref<'a, Scope> {
        self.0.borrow()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
}

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
        expr: Rc<Value>,
    },
    BinOp {
        kind: BinOp,
        left: Thunk,
        right: Thunk,
    },
    Name(String),
    Attribute {
        value: Thunk,
        attr: String,
    },
    Null,
}

#[derive(Clone)]
pub struct Thunk {
    scope: ScopePtr,
    value: Rc<Value>,
    evaluated: Rc<RefCell<Option<Result<Rc<Value>, Error>>>>,
}

// impl From<Value> for Thunk {
//     fn from(v: Value) -> Self {
//         Thunk {
//             value: Rc::new(v),
//             evaluated: RefCell::new(None),
//         }
//     }
// }

impl fmt::Debug for Thunk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.evaluated.borrow() {
            Some(evaluated) => write!(f, "Thunk<done>({:?})", evaluated),
            None => write!(f, "Thunk<pending>({:?})", self.value),
        }
    }
}

impl Thunk {
    fn clone_with_parent_scope(&self, parent: &ScopePtr) -> Thunk {
        println!("THUNK CWPS: {:?}", self);
        Thunk {
            scope: parent.clone(),
            // TODO: can _maybe_ get rid of the parent pointers now?
            value: Rc::new(self.value.clone_with_parent_scope(parent)),
            evaluated: Rc::new(RefCell::new(None)),
        }
    }

    pub fn evaluate(&self) -> Result<Rc<Value>, Error> {
        if self.evaluated.borrow().is_none() {
            let result = self.scope.evaluate(self);
            *self.evaluated.borrow_mut() = Some(result);
        }
        self.evaluated.borrow().clone().unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    ParseError(pest::error::Error<Rule>),
    NameResolutionError(ScopePtr, String),
    AttributeAccessOnBadType(Rc<Value>, String),
    NoSuchAttribute(ScopePtr, String),
    BadFunctionCall,
    UnsupportedOperation(BinOp, Rc<Value>, Rc<Value>),
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(parse_error: pest::error::Error<Rule>) -> Self {
        Error::ParseError(parse_error)
    }
}

use pest::iterators::Pair;

type _BuiltinFn = fn(&ScopePtr, &Vec<Thunk>) -> Result<Rc<Value>, Error>;

#[derive(Clone)]
pub struct BuiltinFn(String, _BuiltinFn);

impl fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "builtin_{}", self.0)
    }
}

fn builtin_bool(_scope: &ScopePtr, args: &Vec<Thunk>) -> Result<Rc<Value>, Error> {
    if args.len() != 1 {
        Err(Error::BadFunctionCall)?;
    }
    Ok(Rc::new(Value::Boolean(
        match &*args.first().unwrap().evaluate()? {
            Value::Object(scope) => scope.borrow().values.len() != 0,
            Value::Array(values) => values.len() != 0,
            Value::Number(num) => *num != 0.,
            Value::Boolean(val) => *val,
            Value::String(val) => val != "",
            Value::Null => false,
            Value::Lambda { .. } => true,
            _ => Err(Error::BadFunctionCall)?,
        },
    )))
}

fn builtin_if(scope: &ScopePtr, args: &Vec<Thunk>) -> Result<Rc<Value>, Error> {
    if let [pred, true_value, false_value] = &args[..] {
        // TODO: I think this is a bug, I don't want to clone a Thunk like this
        let bool_args = vec![pred.clone()];
        let pred = builtin_bool(scope, &bool_args)?;
        match *pred {
            Value::Boolean(true) => true_value.evaluate(),
            Value::Boolean(false) => false_value.evaluate(),
            _ => unreachable!(),
        }
    } else {
        Err(Error::BadFunctionCall)
    }
}

fn builtin_map(scope: &ScopePtr, args: &Vec<Thunk>) -> Result<Rc<Value>, Error> {
    if let [f, array] = &args[..] {
        match &*array.evaluate()? {
            // TODO: Hmm I want to return an un-evaluated array here; will that be a problem?
            //       _I think_ my execution model makes the assumption that .evaluate() is "final" right now.
            Value::Array(values) => {
                let mapped = values
                    .iter()
                    .map(|v| Value::FunctionCall {
                        f: f.clone(),
                        args: vec![v.clone()],
                    })
                    .map(|v| scope.thunk(v));
                Ok(Rc::new(Value::Array(mapped.collect())))
            }
            Value::Object(scope) => {
                let mapped = scope
                    .borrow()
                    .values
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            scope.thunk(Value::FunctionCall {
                                f: f.clone(),
                                args: vec![v.clone()],
                            }),
                        )
                    })
                    .collect();
                Ok(Rc::new(Value::Object(ScopePtr::from_values(
                    mapped,
                    Some(scope.clone()),
                ))))
            }
            _ => Err(Error::BadFunctionCall),
        }
    } else {
        Err(Error::BadFunctionCall)
    }
}

fn builtins() -> ScopePtr {
    let builtins: [(&str, _BuiltinFn); 3] = [
        ("if", builtin_if),
        ("bool", builtin_bool),
        ("map", builtin_map),
    ];
    let values = HashMap::from(builtins.map(|(name, f)| {
        (
            name.into(),
            ScopePtr::new(None).thunk(Value::BuiltinFn(BuiltinFn(name.into(), f))),
        )
    }));
    ScopePtr::from_values(values, None)
}

fn patch(left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
    Ok(
        ScopePtr::new(None).thunk(match (&*left.evaluate()?, &*right.evaluate()?) {
            (_, Value::Lambda { .. } | Value::BuiltinFn(..)) => Value::FunctionCall {
                f: right.clone(),
                args: vec![left.clone()],
            },
            (_, Value::Object(..)) => Value::BinOp {
                kind: BinOp::Plus,
                left: left.clone(),
                right: right.clone(),
            },
            (Value::Patch(l), _) => Value::Patch(patch(l, right)?),
            (l, r) => Err(Error::UnsupportedOperation(
                BinOp::Plus,
                Rc::new(l.clone()),
                Rc::new(r.clone()),
            ))?,
        }),
    )
}

// fn splat_patch(left: &Thunk, right: &Thunk) -> Result<Thunk, Error> {
//     Ok(ScopePtr::new(None).thunk(match &*left.evaluate()? {
//         Value::Array(values) => {
//             let values: Result<Vec<Thunk>, Error> =
//                 values.iter().map(|v| patch(v, right)).collect();
//             Value::Array(values?)
//         }
//         Value::Object(scope) => {
//             let values: Result<HashMap<String, Thunk>, Error> = scope
//                 .borrow()
//                 .values
//                 .iter()
//                 .map(|(k, v)| patch(v, right).and_then(|v| Ok((k.clone(), v))))
//                 .collect();
//             Value::Object(ScopePtr::from_values(
//                 values?,
//                 scope.borrow().parent.clone(),
//             ))
//         }
//         Value::Patch(l) => Value::Patch(splat_patch(l, right)?),
//         _ => Err(Error::UnsupportedOperation(
//             BinOp::Plus,
//             left.value.clone(),
//             right.value.clone(),
//         ))?,
//     }))
// }

fn parse_name_or_string(pair: Pair<Rule>) -> String {
    match pair.as_rule() {
        Rule::name => pair,
        Rule::string => pair.into_inner().next().unwrap(),
        _ => unreachable!("Unexpected rule for parse_name_or_string: {:?}", pair),
    }
    .as_str()
    .into()
}

impl ScopePtr {
    pub fn get(&self, name: &String) -> Option<Thunk> {
        self.borrow().values.get(name).cloned()
    }

    fn sub_scope(&self, values: HashMap<String, Thunk>) -> ScopePtr {
        ScopePtr::from_values(values, Some(self.clone()))
    }

    pub fn thunk(&self, value: Value) -> Thunk {
        Thunk {
            scope: self.clone(),
            value: Rc::new(value),
            evaluated: Rc::new(RefCell::new(None)),
        }
    }

    fn name_lookup(&self, name: &String) -> Option<Thunk> {
        match (self.get(name), &self.borrow().parent) {
            (Some(v), _) => Some(v),
            (None, Some(parent)) => parent.name_lookup(name),
            _ => None,
        }
    }

    fn clone_with_parent_scope(&self, parent: &ScopePtr) -> ScopePtr {
        println!("SCOPE CWPS: {:?}", self);
        let new_scope = ScopePtr::new(Some(parent.clone()));
        let values: HashMap<String, Thunk> = self
            .borrow()
            .values
            .iter()
            .map(|(k, v)| (k.clone(), v.clone_with_parent_scope(&new_scope)))
            .collect();
        new_scope.0.borrow_mut().values = values;
        new_scope
    }

    pub fn evaluate(&self, thunk: &Thunk) -> Result<Rc<Value>, Error> {
        println!("EVALUATE:\n\tvalue: {:?}\n\tscope: {:?}", thunk.value, self);
        match &*thunk.value {
            Value::Object(..)
            | Value::Patch(..)
            | Value::Number(..)
            | Value::String(..)
            | Value::Boolean(..)
            | Value::Null
            | Value::Lambda { .. }
            | Value::BuiltinFn(..) => Ok(thunk.value.clone()),
            Value::Name(name) => {
                let v = self
                    .name_lookup(&name)
                    .ok_or_else(|| Error::NameResolutionError(self.clone(), name.clone()))?;
                v.evaluate()
            }
            Value::Attribute { value, attr } => match &*value.evaluate()? {
                Value::Object(scope) => scope.evaluate(
                    &scope
                        .get(&attr)
                        .ok_or_else(|| Error::NoSuchAttribute(scope.clone(), attr.clone()))?,
                ),
                _ => Err(Error::AttributeAccessOnBadType(
                    value.value.clone(),
                    attr.clone(),
                ))?,
            },
            Value::Array(values) => {
                for v in values.iter() {
                    v.evaluate()?;
                }
                Ok(thunk.value.clone())
            }
            // TODO: I think there's some funkiness with function scope here.
            // We want the function's scope to be its lexical scope, no matter how
            // we get a reference to it.
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
                    scope.thunk(expr.clone_with_parent_scope(&scope)).evaluate()
                }
                Value::BuiltinFn(BuiltinFn(_, f)) => f(self, &args),
                _ => Err(Error::BadFunctionCall),
            },
            Value::BinOp { kind, left, right } => {
                let lv = left.evaluate()?;
                let rv = right.evaluate()?;
                Ok(Rc::new(match kind {
                    BinOp::Plus => {
                        // TODO: any encapsulation at all :P
                        match (&*lv, &*rv) {
                            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                            (Value::String(l), Value::String(r)) => {
                                Value::String(l.to_owned() + &r)
                            }
                            (Value::Array(l), Value::Array(r)) => Value::Array({
                                let mut a = l.clone();
                                a.append(&mut r.clone());
                                a
                            }),
                            (_, Value::Patch(right)) => return patch(left, right)?.evaluate(),
                            (Value::Object(s1), Value::Object(s2)) => {
                                // TODO: what parent to keep here now?
                                // I think the answer is that objects being scopes is outdated.
                                // Thunks have scopes which have parents, but objects/patches shouldn't.
                                let mut values = s1.borrow().values.clone();
                                for (k, v) in s2.borrow().values.iter() {
                                    match &*v.evaluate()? {
                                        Value::Patch(..) => {
                                            let entry = values.entry(k.clone());
                                            entry
                                                .and_modify(|lv| {
                                                    *lv = v.scope.thunk(Value::BinOp {
                                                        kind: BinOp::Plus,
                                                        left: lv.clone(),
                                                        right: v.clone(),
                                                    });
                                                })
                                                .or_insert(v.clone());
                                        }
                                        _ => {
                                            values.insert(k.clone(), v.clone());
                                        }
                                    }
                                }
                                Value::Object(ScopePtr::from_values(
                                    values,
                                    s1.borrow().parent.clone(),
                                ))
                            }
                            // (
                            //     Value::Object(s1) | Value::Patch(s1),
                            //     Value::Object(s2) | Value::Patch(s2),
                            // ) => {
                            //     // TODO: what parent to keep here now?
                            //     // I think the answer is that objects being scopes is outdated.
                            //     // Thunks have scopes which have parents, but objects/patches shouldn't.
                            //     let mut scope = s1.borrow().clone();
                            //     for (k, v) in s2.borrow().values.iter() {
                            //         match &*v.evaluate()? {
                            //             Value::Patch(..) => {
                            //                 let entry = scope.values.entry(k.clone());
                            //                 entry
                            //                     .and_modify(|lv| {
                            //                         *lv = v.scope.thunk(Value::BinOp {
                            //                             kind: BinOp::Plus,
                            //                             left: lv.clone(),
                            //                             right: v.clone(),
                            //                         });
                            //                     })
                            //                     .or_insert(v.clone());
                            //             }
                            //             _ => {
                            //                 scope.values.insert(k.clone(), v.clone());
                            //             }
                            //         }
                            //     }
                            //     match &*left {
                            //         Value::Object(..) => {
                            //             Value::Object(ScopePtr(Rc::new(RefCell::new(scope))))
                            //         }
                            //         Value::Patch(..) => {
                            //             Value::Patch(ScopePtr(Rc::new(RefCell::new(scope))))
                            //         }
                            //         _ => unreachable!(),
                            //     }
                            // }
                            _ => Err(Error::UnsupportedOperation(*kind, lv.clone(), rv.clone()))?,
                        }
                    }
                    _ => todo!(),
                }))
            }
        }
    }
}

impl Value {
    fn clone_with_parent_scope(&self, parent: &ScopePtr) -> Value {
        println!("VALUE CWPS: {:?}", self);
        match self {
            Value::Object(scope) => Value::Object(scope.clone_with_parent_scope(parent)),
            Value::Patch(value) => Value::Patch(value.clone_with_parent_scope(parent)),
            Value::Array(values) => Value::Array(
                values
                    .iter()
                    .map(|v| v.clone_with_parent_scope(parent))
                    .collect(),
            ),
            Value::BinOp { kind, left, right } => Value::BinOp {
                kind: *kind,
                left: left.clone_with_parent_scope(parent),
                right: right.clone_with_parent_scope(parent),
            },
            Value::Attribute { value, attr } => Value::Attribute {
                value: value.clone_with_parent_scope(parent),
                attr: attr.clone(),
            },
            Value::Lambda {
                scope,
                arg_names,
                expr,
            } => {
                let scope = scope.clone_with_parent_scope(parent);
                let expr = Rc::new(expr.clone_with_parent_scope(&scope));
                Value::Lambda {
                    scope,
                    arg_names: arg_names.clone(),
                    expr,
                }
            }
            Value::FunctionCall { f, args } => Value::FunctionCall {
                f: f.clone_with_parent_scope(parent),
                args: args
                    .iter()
                    .map(|v| v.clone_with_parent_scope(parent))
                    .collect(),
            },
            Value::Boolean(..)
            | Value::String(..)
            | Value::Number(..)
            | Value::Name(..)
            | Value::BuiltinFn(..)
            | Value::Null => self.clone(),
        }
    }
}

fn parse_value(pair: Pair<Rule>, scope: &ScopePtr) -> Value {
    // println!(
    //     "PARSING RULE {:?} ({}) IN SCOPE {:?}",
    //     pair.as_rule(),
    //     pair.as_str(),
    //     &scope
    // );
    let rule = pair.as_rule();
    match rule {
        Rule::object => {
            let scope = ScopePtr::new(Some(scope.clone()));
            let pairs = pair.into_inner().map(|pair| {
                let mut inner_rules = pair.into_inner();
                let name = parse_name_or_string(inner_rules.next().unwrap());
                let value = parse_value(inner_rules.next().unwrap(), &scope);
                (name, scope.thunk(value))
            });
            scope.0.borrow_mut().values = pairs.collect();
            Value::Object(scope)
        }
        Rule::array => Value::Array(
            pair.into_inner()
                .map(|p| parse_value(p, scope))
                .map(|v| scope.thunk(v))
                .collect(),
        ),
        Rule::string => Value::String(String::from(pair.into_inner().next().unwrap().as_str())),
        Rule::number => Value::Number(pair.as_str().parse().unwrap()),
        Rule::boolean => Value::Boolean(pair.as_str().parse().unwrap()),
        Rule::null => Value::Null,
        Rule::lambda => {
            let mut inner_rules = pair.into_inner();
            let arg_list = inner_rules.next().unwrap();
            let arg_names: Vec<String> = arg_list.into_inner().map(parse_name_or_string).collect();
            let expr = parse_value(inner_rules.next().unwrap(), scope);
            // Idea:
            //  - create a scope whose parent scope is the lexical scope of the function definition
            //  - when we call the function
            //    1 create a new scope whose parent scope is the function's lexical scope and contains argument values
            //    2 clone the expression tree, but replace parent pointers to point to this new scope
            //    3 evaluate the expression in this new scope
            Value::Lambda {
                scope: scope.sub_scope(HashMap::new()),
                arg_names,
                expr: Rc::new(expr),
            }
        }
        Rule::atom => {
            let mut inner_rules = pair.into_inner();
            let mut value = parse_value(inner_rules.next().unwrap(), scope);
            for pair in inner_rules {
                match pair.as_rule() {
                    Rule::atom_attribute => {
                        let attr = parse_name_or_string(pair.into_inner().next().unwrap());
                        value = Value::Attribute {
                            value: scope.thunk(value),
                            attr,
                        };
                    }
                    Rule::atom_function_call => {
                        // TODO: macro that does something like
                        // let [f, value_list] = unpack_rule!(inner_rules, 2);
                        // or maybe something smarter that lets me match against the Pairs object
                        // and automatically returns an error result if there's a different number of tokens
                        let mut inner_rules = pair.into_inner();
                        let expression_list = inner_rules.next().unwrap();
                        let args: Vec<Thunk> = expression_list
                            .into_inner()
                            .map(|p| parse_value(p, scope))
                            .map(|v| scope.thunk(v))
                            .collect();
                        value = Value::FunctionCall {
                            f: scope.thunk(value),
                            args,
                        }
                    }
                    _ => unreachable!(),
                }
            }
            value
        }
        Rule::name => Value::Name(parse_name_or_string(pair)),
        Rule::plus => {
            let mut inner_rules = pair.into_inner();
            let left = parse_value(inner_rules.next().unwrap(), scope);
            let right = parse_value(inner_rules.next().unwrap(), scope);
            Value::BinOp {
                kind: BinOp::Plus,
                left: scope.thunk(left),
                right: scope.thunk(right),
            }
        }
        Rule::patch => {
            Value::Patch(scope.thunk(parse_value(pair.into_inner().next().unwrap(), scope)))
        }
        Rule::patch_map => {
            // This is horrible :P Basically we're writing a macro here to replace
            // &&x -> &((a) => map((v) => v + &x, a))
            let inner = scope.thunk(parse_value(pair.into_inner().next().unwrap(), scope));
            // TODO: yuck, this and all the CWPS calls are a good indication we're
            // doing something silly
            let new_scope = ScopePtr::new(None);
            let map = Value::Lambda {
                scope: new_scope.clone(),
                arg_names: vec!["a".into()],
                expr: Rc::new(Value::FunctionCall {
                    f: new_scope.thunk(Value::BuiltinFn(BuiltinFn("map".into(), builtin_map))),
                    args: vec![
                        new_scope.thunk(Value::Lambda {
                            scope: new_scope.clone(),
                            arg_names: vec!["v".into()],
                            expr: Rc::new(Value::BinOp {
                                kind: BinOp::Plus,
                                left: new_scope.thunk(Value::Name("v".into())),
                                right: new_scope.thunk(Value::Patch(inner)),
                            }),
                        }),
                        new_scope.thunk(Value::Name("a".into())),
                    ],
                }),
            };
            Value::Patch(new_scope.thunk(map))
        }
        // TODO
        Rule::file
        | Rule::EOI
        | Rule::pair
        | Rule::string_inner
        | Rule::char
        | Rule::arg_list
        | Rule::primitive
        | Rule::atom_attribute
        | Rule::atom_function_call
        | Rule::expression
        | Rule::expression_list
        | Rule::WHITESPACE => unreachable!(),
    }
}

pub fn parse(contents: &str) -> Result<Value, Error> {
    let mut pairs = ConflagParser::parse(Rule::file, contents)?;
    let value = parse_value(pairs.next().unwrap(), &builtins());
    Ok(value)
}
