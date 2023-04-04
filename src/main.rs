use std::env;
use std::fs;

use conflag;

fn main() {
    let args: Vec<String> = env::args().collect();
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
            println!("evaluating: eval = {:?}", value.attr("eval"));
        }
    }
}
