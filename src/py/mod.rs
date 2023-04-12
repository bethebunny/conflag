use std::rc::Rc;

use pyo3::{
    create_exception,
    exceptions::PyException,
    prelude::{pyclass, pyfunction, pymethods, pymodule},
    types::PyModule,
    wrap_pyfunction, IntoPy, PyObject, PyResult, Python,
};

use crate::Value;

create_exception!(conflag, ConflagError, PyException);

// TODO: I hadn't thought about Sync and Send at all yet.
// It's likely this will require some deeper thought about the library structure.
// For now make them unsendable, which is awkward for python users and a potential
// footgun, but we'll figure that out later.
#[pyclass(unsendable, module = "conflag")]
struct ConflagObject(Rc<Value>);

fn as_pyobject(py: Python<'_>, value: Rc<Value>) -> PyObject {
    match &*value.as_ref() {
        Value::Boolean(b) => b.into_py(py),
        Value::Number(n) => n.into_py(py),
        Value::Null => py.None(),
        Value::Object(..) => ConflagObject(value).into_py(py),
        _ => todo!(),
    }
}

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
        Ok(as_pyobject(py, value))
    }

    fn __getitem__(&self, py: Python<'_>, item: &str) -> PyResult<PyObject> {
        self.__getattr__(py, item)
    }
}

#[pyfunction]
fn loads(py: Python<'_>, s: &str) -> PyResult<PyObject> {
    Ok(as_pyobject(py, crate::parse(s)?))
}

#[pymodule]
fn conflag(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<ConflagObject>()?;
    m.add_function(wrap_pyfunction!(loads, m)?)?;
    Ok(())
}
