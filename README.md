# Conflag 🔥🏁
![Auto-deploy](https://github.com/bethebunny/conflag/actions/workflows/ci_tests.yaml/badge.svg)

Conflag is a data language. It's perfect for application configuration, once those configurations grow beyond a few command line flags or a 5-line JSON file.

Example use cases for Conflag:
- **Service deployment** -- eg. replacing json/yaml for Kubernetes or nginx configs
- **Continuous integration** --  eg. replacing yaml for Github Actions
- **ML experiments** -- eg. replacing code for repeatable and replicable experiment parameters)
- **Developer tooling** -- eg. replacing JSON for VS Code or Sublime text configs

Write your data declaratively, but with table-stakes like references, comments, and multiple files.

Any valid JSON file is already a valid Conflag file, so migration is easy, and the syntax is clear and easy to read. Get started in minutes and leave JSON behind.

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

## Meet the language

Write a config file: `object.cfg`
```js
{
    hello: (name) => "Hello, " + name,
    name: "Stef",
    message: hello(name),
    double: (x) => x + x,
    data: map(double, [1, 2, 3, 4, 5]),
}
```

### Read into Rust struct with Serde

```rust
#[derive(serde::Deserializer, Debug)]
struct Stuff {
    message: String,
    data: Vec<u64>,
}

fn main() {
    let raw: &str = fs::read_to_string("object.cfg").unwrap();
    let stuff: Stuff = conflag::serde::from_str(raw).unwrap();
    println!("{stuff:?}");
}
```

### Read from Python

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

## More language details

### A more complex example
The following config will load to a list of model hyperparameter configs,
running a parameter sweep over values for a dropout parameter.

It demonstrates a few features, such as
- the builtin `if` and `map` functions
- anonymous scopes -- the output is an array, rather than an object, and see how the arange function creates an anonymous scope
- patching -- notice how we can easily make variants of `model_params` with fine-grained adjustments
```js
{
    // Base hyperparams for a simple model
    model_params: {
        decoder_mlp: {
            hidden_layers: [1024, 512, 256],
            dropout: 0.2,
            activation: "relu",
            norm: null,
        }
    },
    
    // Let's make a helper function to help us with the sweep.
    // Like numpy's arange, create a list with a range of float values
    arange: (start, stop, step) => {
        // Name an internal computation in an anonymous scope
        done: if(step > 0, start >= stop, start <= stop),
        // Recursive calls allow building any necessary helpers
        result: if(done, [], [start] + arange(start + step, stop, step)),
    }.result,
    
    // Create a list that contains a copy of model_params for each value in our sweep
    experiments: map(
        // & is the patch operator, letting us override just the parts we want to change
        (_dropout) => model_params + {decoder_mlp: &{dropout: _dropout}},
        arange(0, 0.4, 0.01),
    ),

// We don't need to expose any of the other junk to our program!
// The data hides it in this anonymous scope and only produces the output.
}.experiments
```
