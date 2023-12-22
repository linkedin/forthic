mod errors;
mod token;
mod tokenizer;

use tokenizer::Tokenizer;

fn main() {
    let mut lexer = Tokenizer::new("Hello, world!");
    let t = lexer.next_token().unwrap();
    println!("{:?}", t);
}
