# conflag
A functional and declarative configuration language. A superset of JSON with named references, scopes, functions, and other cool features. Bindings for python and Rust, with Rust Serde integration.

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
>>> data
[2, 4, 6, 8, 10]
```
