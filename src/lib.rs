extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use pest::Parser;

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
// - derive(FromConfig)

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

impl fmt::Debug for ScopePtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

    pub fn get(&self, name: &String) -> Option<Thunk> {
        self.borrow().values.get(name).cloned()
    }

    fn sub_scope(&self, values: HashMap<String, Thunk>) -> ScopePtr {
        ScopePtr::from_values(values, Some(self.clone()))
    }

    fn name_lookup(&self, name: &String) -> Option<Thunk> {
        match (self.get(name), &self.borrow().parent) {
            (Some(v), _) => Some(v),
            (None, Some(parent)) => parent.name_lookup(name),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Patch,
    ObjectReplace,
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
        expr: Rc<AstNode>,
    },
    BinOp {
        kind: BinOp,
        left: Thunk,
        right: Thunk,
    },
    Name(ScopePtr, String),
    Attribute {
        value: Thunk,
        attr: String,
    },
    Null,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Object(HashMap<String, AstNode>),
    Patch(Box<AstNode>),
    Array(Vec<AstNode>),
    Number(f64),
    Boolean(bool),
    Null,
    String(String),
    Name(String),
    Attribute {
        value: Box<AstNode>,
        attr: String,
    },
    FunctionCall {
        f: Box<AstNode>,
        args: Vec<AstNode>,
    },
    Lambda {
        arg_names: Vec<String>,
        expr: Rc<AstNode>,
    },
    BinOp {
        kind: BinOp,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
}

impl Value {
    fn is_primitive(&self) -> bool {
        match self {
            Value::Object(..)
            | Value::Array(..)
            | Value::Patch(..)
            | Value::Number(..)
            | Value::String(..)
            | Value::Boolean(..)
            | Value::Null
            | Value::Lambda { .. }
            | Value::BuiltinFn(..) => true,
            _ => false,
        }
    }
}

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

    fn evaluated<'a>(&'a self) -> Ref<'a, RefCell<Option<Result<Rc<Value>, Error>>>> {
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
                _ => todo!(),
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
            Value::BinOp { kind, left, right } => {
                kind.evaluate(&left.evaluate()?, &right.evaluate()?)
            }
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

use pest::iterators::Pair;

type _BuiltinFn = fn(&Vec<Thunk>) -> Result<Thunk, Error>;

#[derive(Clone)]
pub struct BuiltinFn(pub String, pub _BuiltinFn);

impl From<BuiltinFn> for Thunk {
    fn from(value: BuiltinFn) -> Self {
        Value::BuiltinFn(value).into()
    }
}

impl fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "builtin_{:?}", self.0)
    }
}

// We should really re-write these as async functions :/
fn builtin_bool(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if args.len() != 1 {
        Err(Error::BadFunctionCall)?;
    }
    let v = args.first().unwrap().evaluate();
    Ok(Value::Boolean(match &*v.clone()? {
        Value::Object(scope) => scope.borrow().values.len() != 0,
        Value::Array(values) => values.len() != 0,
        Value::Number(num) => *num != 0.,
        Value::Boolean(val) => *val,
        Value::String(val) => val != "",
        Value::Null => false,
        Value::Lambda { .. } => true,
        _ => Err(Error::BadFunctionCall)?,
    })
    .into())
}

fn builtin_if(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if let [pred, true_value, false_value] = &args[..] {
        let bool_args = vec![pred.clone()];
        let pred = builtin_bool(&bool_args)?;
        Ok(match &*pred.evaluate()? {
            Value::Boolean(true) => true_value.clone(),
            Value::Boolean(false) => false_value.clone(),
            _ => unreachable!(),
        })
    } else {
        Err(Error::BadFunctionCall)
    }
}

fn builtin_map(args: &Vec<Thunk>) -> Result<Thunk, Error> {
    if let [f, array] = &args[..] {
        match &*array.evaluate()? {
            // TODO: Hmm I want to return an un-evaluated array here; will that be a problem?
            //       _I think_ my execution model makes the assumption that .evaluate() is "final" right now.
            Value::Array(values) => {
                let mapped = values.iter().map(|v| {
                    Value::FunctionCall {
                        f: f.clone(),
                        args: vec![v.clone()],
                    }
                    .into()
                });
                Ok(Value::Array(mapped.collect()).into())
            }
            Value::Object(scope) => {
                let mapped = scope
                    .borrow()
                    .values
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            Value::FunctionCall {
                                f: f.clone(),
                                args: vec![v.clone()],
                            }
                            .into(),
                        )
                    })
                    .collect();
                Ok(Value::Object(ScopePtr::from_values(mapped, Some(scope.clone()))).into())
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
    let values =
        HashMap::from(builtins.map(|(name, f)| (name.into(), BuiltinFn(name.into(), f).into())));
    ScopePtr::from_values(values, None)
}

impl BinOp {
    fn evaluate(&self, left: &Rc<Value>, right: &Rc<Value>) -> Result<Thunk, Error> {
        match self {
            BinOp::Plus => self.evaluate_plus(left, right),
            // BinOp::Patch => patch(left, right),
            // BinOp::ObjectReplace => patch(left, right),
            _ => unimplemented!(),
        }
    }

    fn evaluate_plus(&self, left: &Rc<Value>, right: &Rc<Value>) -> Result<Thunk, Error> {
        let value = match (&**left, &**right) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (Value::String(l), Value::String(r)) => Value::String(l.to_owned() + &r),
            (Value::Array(l), Value::Array(r)) => Value::Array({
                let mut a = l.clone();
                a.append(&mut r.clone());
                a
            }),
            (_, Value::Patch(right)) => Value::BinOp {
                kind: BinOp::Patch,
                left: left.clone().into(),
                right: right.clone(),
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
            _ => Err(Error::UnsupportedOperation(
                *self,
                left.clone(),
                right.clone(),
            ))?,
        };
        Ok(value.into())
    }

    fn patch(left: &Rc<Value>, right: &Rc<Value>) -> Result<Thunk, Error> {
        Ok(match (&**left, &**right) {
            (_, Value::Lambda { .. } | Value::BuiltinFn(..)) => Value::FunctionCall {
                f: right.clone().into(),
                args: vec![left.clone().into()],
            },
            (_, Value::Object(..)) => Value::BinOp {
                kind: BinOp::Plus,
                left: left.clone().into(),
                right: right.clone().into(),
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
                left.clone(),
                right.clone(),
            ))?,
        }
        .into())
    }
}

impl AstNode {
    fn value(&self, scope: &ScopePtr) -> Value {
        match self {
            AstNode::Object(values) => {
                let new_scope = ScopePtr::new(Some(scope.clone()));
                let values = values
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone().value(&new_scope).into()))
                    .collect();
                (*new_scope.0.borrow_mut()).values = values;
                Value::Object(new_scope)
            }
            AstNode::Array(values) => {
                let values = values
                    .iter()
                    .map(|v| v.clone().value(scope).into())
                    .collect();
                Value::Array(values)
            }
            AstNode::Number(n) => Value::Number(*n),
            AstNode::Boolean(b) => Value::Boolean(*b),
            AstNode::String(s) => Value::String(s.clone()),
            AstNode::Null => Value::Null,
            AstNode::Name(name) => Value::Name(scope.clone(), name.clone()),
            AstNode::Patch(v) => Value::Patch(v.value(scope).into()),
            AstNode::Lambda { arg_names, expr } => Value::Lambda {
                scope: scope.clone(),
                arg_names: arg_names.clone(),
                expr: expr.clone(),
            },
            AstNode::FunctionCall { f, args } => Value::FunctionCall {
                f: f.value(scope).into(),
                args: args.iter().map(|v| v.clone().value(scope).into()).collect(),
            },
            AstNode::BinOp { kind, left, right } => Value::BinOp {
                kind: *kind,
                left: left.value(scope).into(),
                right: right.value(scope).into(),
            },
            AstNode::Attribute { value, attr } => Value::Attribute {
                value: value.value(scope).into(),
                attr: attr.clone(),
            },
        }
    }

    fn parse_name_or_string(pair: Pair<Rule>) -> String {
        match pair.as_rule() {
            Rule::name => pair,
            Rule::string => pair.into_inner().next().unwrap(),
            _ => unreachable!("Unexpected rule for parse_name_or_string: {:?}", pair),
        }
        .as_str()
        .into()
    }

    fn parse_value(pair: Pair<Rule>) -> Result<AstNode, Error> {
        // println!(
        //     "PARSING RULE {:?} ({}) IN SCOPE {:?}",
        //     pair.as_rule(),
        //     pair.as_str(),
        //     &scope
        // );
        let rule = pair.as_rule();
        let node = match rule {
            Rule::object => {
                let pairs: Result<HashMap<String, AstNode>, Error> = pair
                    .into_inner()
                    .map(|pair| {
                        let mut inner_rules = pair.into_inner();
                        let name = AstNode::parse_name_or_string(inner_rules.next().unwrap());
                        let value = AstNode::parse_value(inner_rules.next().unwrap());
                        value.and_then(|v| Ok((name, v)))
                    })
                    .collect();
                AstNode::Object(pairs?)
            }
            Rule::array => {
                let values: Result<Vec<AstNode>, Error> =
                    pair.into_inner().map(AstNode::parse_value).collect();
                AstNode::Array(values?)
            }
            Rule::string => {
                AstNode::String(String::from(pair.into_inner().next().unwrap().as_str()))
            }
            Rule::number => AstNode::Number(pair.as_str().parse().unwrap()),
            Rule::boolean => AstNode::Boolean(pair.as_str().parse().unwrap()),
            Rule::null => AstNode::Null,
            Rule::lambda => {
                let mut inner_rules = pair.into_inner();
                let arg_list = inner_rules.next().unwrap();
                let arg_names: Vec<String> = arg_list
                    .into_inner()
                    .map(AstNode::parse_name_or_string)
                    .collect();
                let expr = AstNode::parse_value(inner_rules.next().unwrap())?;
                // Idea:
                //  - create a scope whose parent scope is the lexical scope of the function definition
                //  - when we call the function
                //    1 create a new scope whose parent scope is the function's lexical scope and contains argument values
                //    2 clone the expression tree, but replace parent pointers to point to this new scope
                //    3 evaluate the expression in this new scope
                AstNode::Lambda {
                    arg_names,
                    expr: Rc::new(expr),
                }
            }
            Rule::atom => {
                let mut inner_rules = pair.into_inner();
                let mut value = AstNode::parse_value(inner_rules.next().unwrap())?;
                for pair in inner_rules {
                    match pair.as_rule() {
                        Rule::atom_attribute => {
                            let attr =
                                AstNode::parse_name_or_string(pair.into_inner().next().unwrap());
                            value = AstNode::Attribute {
                                value: Box::new(value),
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
                            let args: Result<Vec<AstNode>, Error> = expression_list
                                .into_inner()
                                .map(AstNode::parse_value)
                                .collect();
                            value = AstNode::FunctionCall {
                                f: Box::new(value),
                                args: args?,
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                value
            }
            Rule::name => AstNode::Name(AstNode::parse_name_or_string(pair)),
            Rule::plus => {
                let mut inner_rules = pair.into_inner();
                let left = AstNode::parse_value(inner_rules.next().unwrap())?;
                let right = AstNode::parse_value(inner_rules.next().unwrap())?;
                AstNode::BinOp {
                    kind: BinOp::Plus,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
            Rule::patch => AstNode::Patch(Box::new(AstNode::parse_value(
                pair.into_inner().next().unwrap(),
            )?)),
            Rule::patch_map => {
                // This is horrible :P Basically we're writing a macro here to replace
                // &&x -> &((a) => map((v) => v + &x, a))
                let inner = AstNode::parse_value(pair.into_inner().next().unwrap())?;
                let map = AstNode::Lambda {
                    arg_names: vec!["a".into()],
                    expr: Rc::new(AstNode::FunctionCall {
                        // TODO: we want this to always be builtin map, it shouldn't be possible to scope-shadow it
                        f: Box::new(AstNode::Name("map".into())),
                        args: vec![
                            AstNode::Lambda {
                                arg_names: vec!["v".into()],
                                expr: Rc::new(AstNode::BinOp {
                                    kind: BinOp::Plus,
                                    left: Box::new(AstNode::Name("v".into())),
                                    right: Box::new(AstNode::Patch(Box::new(inner))),
                                }),
                            },
                            AstNode::Name("a".into()),
                        ],
                    }),
                };
                AstNode::Patch(Box::new(map))
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
        };
        Ok(node)
    }
}

pub fn parse(contents: &str) -> Result<Rc<Value>, Error> {
    let mut pairs = ConflagParser::parse(Rule::file, contents)?;
    let ast = AstNode::parse_value(pairs.next().unwrap())?;
    Thunk::from(ast.value(&builtins())).evaluate()
}
