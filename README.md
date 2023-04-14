# 🔥🏁 conflag
A functional and declarative configuration language. A superset of JSON with named references, scopes, functions, and other cool features. Bindings for python and Rust, with Rust Serde integration.

![Auto-deploy](https://github.com/bethebunny/conflag/actions/workflows/ci_tests.yaml/badge.svg)

## Getting started

Rust with Serde:
```
cargo add conflag --features serde
```

Python:
```
git clone https://github.com/bethebunny/conflag
cd conflag
cargo build --release --features python
cp target/release/libconflag.so conflag.so
```

## Conflag language

In `object.cfg`
```
{
    hello: (name) => "Hello, " + name,
    name: "Stef",
    message: hello(name),
    double: (x) => x + x,
    data: map(double, [1, 2, 3, 4, 5]),
}
```

Some other language features:
- First class functions and scopes
- Import to break configs into multiple files
- Patches, a new language feature

See more examples of features in the tests directory (I recommend test_fib.cfg, test_cards.cfg, and test_patch.cfg as nice feature overviews).

## Rust serde usage

```rust
#[derive(serde::Deserializer, Debug)]
struct MyStruct {
    message: String,
    data: Vec<u64>,
}

fn main() {
    let s: MyStruct = conflag::serde::from_str(fs::read_to_string("object.cfg"));
    println!("{s:?}");
}
```

## Python usage

```python
>>> import conflag
>>> s = conflag.loads(open("object.cfg").read())
>>> s.name
"Stef"
>>> s.message
"Hello, Stef"
>>> s.data
[2, 4, 6, 8, 10]
```
