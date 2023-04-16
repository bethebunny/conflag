# Conflag 🔥🏁
![Auto-deploy](https://github.com/bethebunny/conflag/actions/workflows/ci_tests.yaml/badge.svg)

Conflag is a data language. It's perfect for application configuration, once those configurations grow beyond a few command line flags or a 5-line JSON file.

Example use cases for Conflag:
- **Service deployment** -- eg. replacing json/yaml for Kubernetes or nginx configs
- **Continuous integration** --  eg. replacing yaml for Github Actions
- **ML experiments** -- repeatable and replicable experiment parameters
- **Developer tooling** -- eg. replacing JSON for VS Code or Sublime text configs

Write your data declaratively, but with table-stakes features like references, comments, and multiple files.

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
    
    // A helper function to help us with the sweep.
    // Like numpy's arange, create a list with a range of float values
    arange: (start, stop, step) => {
        // Name an internal computation in an anonymous scope
        done: if(step > 0, start >= stop, start <= stop),
        // Recursive calls allow building any necessary helpers
        result: if(done, [], [start] + arange(start + step, stop, step)),
    }.result,
    
    // A list that contains a copy of model_params for each value in our sweep
    experiments: map(
        // & is the patch operator, letting us override just the parts we want to change
        (_dropout) => model_params + {decoder_mlp: &{dropout: _dropout}},
        arange(0, 0.4, 0.01),
    ),

// We don't need to expose any of the other junk to our program!
// The data hides it in this anonymous scope and only produces the output list.
}.experiments
```

### The Patch operator

The unary Patch operator `&` is a feature novel to Conflag. Its behavior is easier to
explain with examples, but fundamentally it allows you to customize what happens *when
a value is replaced in a scope*.

For example,

```js
{a: 1} + {a: 2}
```

The value of this expression is `{a: 2}`, because the `a` in the second object
*replaces* the value of `a` in the first object.

With the patch operator, we can configure this behavior in a couple of ways.
Syntactically, this looks like:
```js
{a: u} + {a: &v}
```

For instance, compare with and without the patch operator
```js
{
  // has value {a: {b: "replace"}}
  without_patch: {a: {b: "b", c: "c"}} +  {a: {b: "replace"}}

  // has value {a: {b: "replace", c: "c"}}
  with_patch:    {a: {b: "b", c: "c"}} + &{a: {b: "replace"}}
}
```

The `+` operator gives a shallow dict union, and recursively it's simple to implement
a full `deep_union(d1, d2)`; however, in configuration it's frequently the case that you
want some mix of the two. Consider the case where you're overriding parameters for an ML
model: you might want to, deep inside a config, change an entire layer to another layer
type, or replace the optimizer with one with entirely different parameters. By placing
(or not placing) the `&` operator, you have infinite flexible control over dict union
operations.

```js
{
    base_model: {
        parameters: {layers: 6},
        training: {
            epochs: 10,
            optimizer: {sgd: {lr: 1e-3, momentum: 1e-6}},
        },
    },
    experiments: [
      base_model,
      base_model + {training: &{optimizer: &{sgd: &{lr: 2e-3}}}},
      base_model + {training: &{optimizer: {adam: {lr: 1e-3, eps: 1e-7}}}},
   ]
}
```

You can also use patches with functions:
```js
base_model + {training: &{epochs: &(epochs) => 2 * epochs}}
```

The patch operator behaves in the following ways:
- If `v` is a function, the result is `{a: v(u)}`
- If `v` and `u` are both objects, the result is `{a: u + v}`
- `{a: &u} + {a: &v}` is equivalent to `{a: &(u + &v)}`
Additionally, `&` has a unique interaction with the `+` operator:
- `u + &v` is equivalent to `({a: u} + {a: &v}).a`

Serendipitously, this means that `+&` is equivalent to the `|>` pipeline operator some
languages are adopting!

```js
{
  point: (_x, _y) => {
    x: _x,
    y: _y,
    math: import("math.cfg"),
    magnitude: math.sqrt(x * x + y + y),
  },
  some_point: point(1, -1)
    +& displayed
    +& (p) => p.magnitude,
}
```
