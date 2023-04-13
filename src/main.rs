use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let [_, files @ ..] = &args[..] {
        for file in files {
            let contents = fs::read_to_string(file).unwrap();
            let value = conflag::parse(&contents).unwrap_or_else(|e| panic!("{}", e));
            println!("{}", value);
        }
    }
}
