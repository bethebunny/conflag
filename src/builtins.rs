use core::fmt;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{ast::AstNode, scope::ScopePtr, thunk::Thunk, value::Value, Error, Result};

// TODO:
// - rewrite builtins using from_N interface
// - signatures / somehow docs in Display
// - unified Display for native fns and lambdas
// - name consistency: builtin vs native fn

type _BuiltinFn = Rc<dyn Fn(&[Thunk]) -> Result<Thunk>>;

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

impl BuiltinFn {
    pub(crate) fn from_1<F>(name: &str, argnames: &str, f: F) -> Self
    where
        F: Fn(&Thunk) -> Result<Thunk> + 'static,
    {
        let signature = format!("{name}({argnames})");
        BuiltinFn(
            name.into(),
            Rc::new(move |args: &[Thunk]| {
                if let [v] = args {
                    f(v)
                } else {
                    builtin_invalid_args(signature.as_str(), args)
                }
            }),
        )
    }
}

fn builtin_invalid_args<K>(name: &str, args: &[Thunk]) -> Result<K> {
    Err(Error::BuiltinInvalidArguments(name.into(), args.to_vec()))
}

fn builtin_bool(args: &[Thunk]) -> Result<Thunk> {
    if let [v] = args {
        Ok(Value::Boolean(match &*v.evaluate()? {
            Value::Object(scope) => !scope.values().is_empty(),
            Value::Array(values) => !values.is_empty(),
            Value::Number(num) => *num != 0.,
            Value::Boolean(val) => *val,
            Value::String(val) => !val.is_empty(),
            Value::Null => false,
            Value::Lambda { .. } => true,
            _ => Err(Error::TypeError("bool not supported".into(), v.clone()))?,
        })
        .into())
    } else {
        builtin_invalid_args("bool(v)", args)
    }
}

fn builtin_if(args: &[Thunk]) -> Result<Thunk> {
    if let [pred, true_value, false_value] = args {
        let bool_args = vec![pred.clone()];
        let pred = builtin_bool(&bool_args)?;
        Ok(match &*pred.evaluate()? {
            Value::Boolean(true) => true_value.clone(),
            Value::Boolean(false) => false_value.clone(),
            _ => unreachable!(),
        })
    } else {
        builtin_invalid_args("if(pred, t, f)", args)
    }
}

fn builtin_map(args: &[Thunk]) -> Result<Thunk> {
    if let [f, array] = args {
        match &*array.evaluate()? {
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
                    .values()
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
            _ => Err(Error::TypeError(
                "map unsupported over".into(),
                array.clone(),
            )),
        }
    } else {
        builtin_invalid_args("map(f, array | object)", args)
    }
}

fn builtin_reduce(args: &[Thunk]) -> Result<Thunk> {
    if let [f, array, state] = args {
        match &*array.evaluate()? {
            Value::Array(values) => {
                let mut state = state.clone();
                for v in values.iter() {
                    state = Value::FunctionCall {
                        f: f.clone(),
                        args: vec![state.clone(), v.clone()],
                    }
                    .into();
                }
                Ok(state)
            }
            _ => Err(Error::TypeError(
                "reduce unsupported over".into(),
                array.clone(),
            )),
        }
    } else {
        builtin_invalid_args("reduce(f, array, initial_state)", args)
    }
}

fn builtin_displayed(args: &[Thunk]) -> Result<Thunk> {
    if let [v] = args {
        println!("{}", v);
        Ok(v.clone())
    } else {
        builtin_invalid_args("displayed(value)", args)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ImportContext {
    base_dir: PathBuf,
    dir: PathBuf,
}

impl ImportContext {
    pub(crate) fn from_env() -> Self {
        Self::from_base(std::env::current_dir().unwrap())
    }

    pub(crate) fn from_base<P>(base_dir: P) -> Self
    where
        P: AsRef<Path>,
    {
        ImportContext {
            base_dir: base_dir.as_ref().to_path_buf(),
            dir: base_dir.as_ref().to_path_buf(),
        }
    }

    fn with_dir<P>(&self, dir: P) -> Self
    where
        P: AsRef<Path>,
    {
        ImportContext {
            base_dir: self.base_dir.clone(),
            dir: dir.as_ref().to_path_buf(),
        }
    }

    fn import<P>(&self, path: P) -> Result<Thunk>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        let lib = crate::stdlib::modules();
        if let Some(module) = path.to_str().and_then(|p| lib.get(p)) {
            return Ok(module.clone());
        }
        for dir in [&self.dir, &self.base_dir] {
            let path = dir.join(path);
            let contents =
                fs::read_to_string(&path).map_err(|e| Error::ImportReadError(Rc::new(e)));
            match contents {
                Ok(contents) => {
                    let node = AstNode::parse(contents.as_str())?;
                    // path.parent() will be none for a relative import with no directory,
                    // eg. "something.cfg", in which case we want to keep the same import directory context.
                    let ctx = match path.parent() {
                        Some(parent) => self.with_dir(parent),
                        None => self.clone(),
                    };
                    return Ok(node.value(&builtins(ctx)).into());
                }
                Err(_) => continue,
            }
        }
        Err(Error::Custom(format!(
            "Couldn't find a valid import for {path:?} given {self:?}"
        )))
    }

    fn into_builtin(self) -> BuiltinFn {
        BuiltinFn::from_1("import", "path", move |thunk: &Thunk| {
            match &**thunk.evaluate().as_ref()? {
                Value::String(path) => self.import(Path::new(path)),
                _ => Err(Error::TypeError(
                    "import path must be a string".into(),
                    thunk.clone(),
                )),
            }
        })
    }
}

pub(crate) fn builtins(ctx: ImportContext) -> ScopePtr {
    let builtins = [
        ("if", builtin_if as fn(&[Thunk]) -> Result<Thunk>),
        ("bool", builtin_bool),
        ("map", builtin_map),
        ("reduce", builtin_reduce),
        ("displayed", builtin_displayed),
    ];
    let mut values = HashMap::from(
        builtins.map(|(name, f)| (name.into(), BuiltinFn(name.into(), Rc::new(f)).into())),
    );
    values.insert("import".into(), Value::BuiltinFn(ctx.into_builtin()).into());
    ScopePtr::from_values(values, None)
}

#[cfg(test)]
mod test {
    #[test]
    fn test_import_native_stdlib() {
        let imported = crate::parse(r#"import("math")"#).unwrap();
        assert!(imported.attr("sqrt").is_ok());
    }

    #[test]
    fn test_import_conflag_stdlib() {
        let imported = crate::parse(r#"import("core")"#).unwrap();
        assert!(imported.attr("and").is_ok());
    }
}
