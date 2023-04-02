use std::env;
use std::fs;

use conflag;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("size_of(Value): {}", std::mem::size_of::<conflag::Value>());
    println!("size_of(Thunk): {}", std::mem::size_of::<conflag::Thunk>());
    if let [_, files @ ..] = &args[..] {
        for file in files {
            println!("\n\nParsing {}", file);
            let contents = fs::read_to_string(file).unwrap();
            let value = match conflag::parse(&contents) {
                Ok(value) => value,
                Err(e) => {
                    if let conflag::Error::ParseError(p) = &e {
                        println!("Error! {}", p);
                    }
                    panic!("{:?}", e);
                }
            };
            // println!("Value: {:?}", value);
            match value {
                conflag::Value::Object(scope) => {
                    let v = scope.thunk(conflag::Value::Name("eval".into())).evaluate();
                    println!("evaluating: eval = {:?}", v);
                }
                _ => (),
            }
        }
    }
}
