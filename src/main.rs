use std::env;
use std::fs;

use conflag;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let [_, files @ ..] = &args[..] {
        for file in files {
            // println!("\n\nParsing {}", file);
            let contents = fs::read_to_string(file).unwrap();
            let value = conflag::parse(&contents).unwrap_or_else(|e| panic!("{}", e));
            // println!("Value: {:?}", value);
            // println!("evaluating: eval = {:?}", value.attr("eval"));
            println!("{}", value);

            let v: u8 = conflag::serde::from_str("{a: b+1, b: 3}.a").unwrap();
            println!("{v}");
        }
    }
}
