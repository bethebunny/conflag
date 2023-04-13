extern crate pest;

use std::{collections::HashMap, rc::Rc};

use pest::{iterators::Pair, Parser};

use crate::{
    binop::{BinOp, Comparison},
    scope::ScopePtr,
    value::Value,
    Error,
};

#[derive(Parser)]
#[grammar = "conflag.pest"]
pub struct ConflagParser;

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

impl AstNode {
    pub fn parse(s: &str) -> Result<AstNode, Error> {
        let mut pairs = ConflagParser::parse(Rule::file, s)?;
        AstNode::parse_value(pairs.next().unwrap())
    }

    pub fn value(&self, scope: &ScopePtr) -> Value {
        match self {
            AstNode::Object(values) => {
                let mut new_scope = ScopePtr::new(Some(scope.clone()));
                let values = values
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone().value(&new_scope).into()))
                    .collect();
                // Safety: This is the one place we set values on a scope.
                // There very possibly _are_ other references to the pointer (eg. functions, names)
                // but they are guaranteed to not be setting values also, since no other calls do.
                unsafe { new_scope.set_values(values) };
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

    pub fn parse_value(pair: Pair<Rule>) -> Result<AstNode, Error> {
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
                        value.map(|v| (name, v))
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
            // TODO: operator precedence
            Rule::binops => {
                let mut inner_rules = pair.into_inner();
                let mut left_node = AstNode::parse_value(inner_rules.next().unwrap())?;
                while let Some(binop) = inner_rules.next() {
                    let kind = match binop.as_str() {
                        "+" => BinOp::Plus,
                        "-" => BinOp::Subtract,
                        "*" => BinOp::Multiply,
                        "/" => BinOp::Divide,
                        "==" => BinOp::Compare(Comparison::Equal),
                        "!=" => BinOp::Compare(Comparison::NotEqual),
                        ">" => BinOp::Compare(Comparison::GreaterThan),
                        ">=" => BinOp::Compare(Comparison::GreaterThanOrEqual),
                        "<" => BinOp::Compare(Comparison::LessThan),
                        "<=" => BinOp::Compare(Comparison::LessThanOrEqual),
                        _ => unreachable!("Unexpected binop literal '{}'", binop.as_str()),
                    };
                    let right_node = AstNode::parse_value(inner_rules.next().unwrap())?;
                    left_node = AstNode::BinOp {
                        kind,
                        left: Box::new(left_node),
                        right: Box::new(right_node),
                    };
                }
                left_node
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
            Rule::file
            | Rule::EOI
            | Rule::pair
            | Rule::string_inner
            | Rule::char
            | Rule::arg_list
            | Rule::primitive
            | Rule::binop
            | Rule::atom_attribute
            | Rule::atom_function_call
            | Rule::expression
            | Rule::expression_list
            | Rule::COMMENT
            | Rule::WHITESPACE => unreachable!(),
        };
        Ok(node)
    }
}
