mod parser;
mod utils;

use std::collections::HashMap;
use parser::Token;
use utils::{trim_whitespace, repeat};

pub fn build_index(items: &[&str]) -> HashMap<String, usize> {
    let mut map = HashMap::new();
    for (i, item) in items.iter().enumerate() {
        map.insert(item.to_string(), i);
    }
    map
}

pub fn process(input: &str) -> String {
    let parsed = parser::parse(input);
    trim_whitespace(&parsed)
}

pub fn make_token(s: &str) -> Token {
    Token::new("word", s)
}

pub fn shout(s: &str) -> String {
    repeat(s, 3)
}
