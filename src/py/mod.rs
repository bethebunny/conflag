use std::{collections::hash_map::Keys, rc::Rc};

use pyo3::{
    create_exception,
    exceptions::{PyException, PyIndexError},
    prelude::{pyclass, pyfunction, pymethods, pymodule},
    types::{PyMapping, PyModule, PySequence, PySlice},
    wrap_pyfunction, FromPyObject, IntoPy, PyObject, PyResult, Python,
};

use crate::{thunk::Thunk, Error, Value};

create_exception!(conflag, ConflagError, PyException);

fn as_pyobject(py: Python<'_>, value: Rc<Value>) -> PyResult<PyObject> {
    Ok(match &*value.as_ref() {
        Value::Boolean(b) => b.into_py(py),
        Value::Number(n) => n.into_py(py),
        Value::Null => py.None(),
        Value::String(s) => s.into_py(py),
        Value::Object(..) => ConflagObject(value).into_py(py),
        Value::Array(..) => ConflagArray(value).into_py(py),
        v => Err(Error::Custom(format!(
            "No native python type for conflag primitive: {v}"
        )))?,
    })
}

// TODO: I hadn't thought about Sync and Send at all yet.
// It's likely this will require some deeper thought about the library structure.
// For now make them unsendable, which is awkward for python users and a potential
// footgun, but we'll figure that out later.

// contained value will always be a Value::Object
#[pyclass(unsendable, mapping, frozen, module = "conflag")]
struct ConflagObject(Rc<Value>);

#[pymethods]
impl ConflagObject {
    fn __str__(&self) -> PyResult<String> {
        Ok(format!("{}", self.0.as_ref()))
    }

    fn __repr__(&self) -> PyResult<String> {
        Ok(format!("{}", self.0.as_ref()))
    }

    fn __getattr__(&self, py: Python<'_>, attr: &str) -> PyResult<PyObject> {
        let value = self.0.attr(attr)?;
        as_pyobject(py, value)
    }

    fn __getitem__(&self, py: Python<'_>, item: &str) -> PyResult<PyObject> {
        self.__getattr__(py, item)
    }

    fn __len__(&self) -> PyResult<usize> {
        if let Value::Object(scope) = &*self.0.as_ref() {
            Ok(scope.values().len())
        } else {
            unreachable!();
        }
    }

    // fn keys(&self, py: Python<'_>) -> PyResult<PyObject> {
    //     if let Value::Object(scope) = &*self.0.as_ref() {
    //         Ok(scope.values().keys().into_py(py))
    //     } else {
    //         unreachable!();
    //     }
    // }

    // fn __dir__(..)
}

// contained value will always be a Value::Array
#[pyclass(unsendable, sequence, frozen, module = "conflag")]
struct ConflagArray(Rc<Value>);

#[derive(FromPyObject)]
enum SliceOrInt<'a> {
    Slice(&'a PySlice),
    Int(isize),
}

#[pymethods]
impl ConflagArray {
    fn __str__(&self) -> PyResult<String> {
        Ok(format!("{}", self.0.as_ref()))
    }

    fn __repr__(&self) -> PyResult<String> {
        Ok(format!("{}", self.0.as_ref()))
    }

    fn __getitem__(&self, py: Python<'_>, item: SliceOrInt) -> PyResult<PyObject> {
        if let Value::Array(values) = &*self.0.as_ref() {
            match item {
                // TODO: we really need to figure out a good way to test this :P
                SliceOrInt::Slice(slice) => {
                    let indices = slice.indices(values.len() as i64)?;
                    let mut result = Vec::with_capacity(indices.slicelength as usize);
                    let mut idx = indices.start;
                    while if indices.step > 0 {
                        idx < indices.stop
                    } else {
                        idx > indices.stop
                    } {
                        result.push(values[idx as usize].clone());
                        idx += indices.step;
                    }
                    as_pyobject(py, Rc::new(Value::Array(result)))
                }
                SliceOrInt::Int(idx) => {
                    let idx = if idx < 0 {
                        idx + values.len() as isize
                    } else {
                        idx
                    } as usize;
                    if idx >= values.len() {
                        Err(PyIndexError::new_err(idx))
                    } else {
                        as_pyobject(py, values[idx].evaluate()?)
                    }
                }
            }
        } else {
            unreachable!();
        }
    }

    fn __len__(&self) -> PyResult<usize> {
        if let Value::Array(values) = &*self.0.as_ref() {
            Ok(values.len())
        } else {
            unreachable!();
        }
    }
}

#[pyfunction]
fn loads(py: Python<'_>, s: &str) -> PyResult<PyObject> {
    as_pyobject(py, crate::parse(s)?)
}

#[pymodule]
fn conflag(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<ConflagObject>()?;
    m.add_function(wrap_pyfunction!(loads, m)?)?;
    PyMapping::register::<ConflagObject>(py)?;
    PySequence::register::<ConflagArray>(py)?;
    Ok(())
}
