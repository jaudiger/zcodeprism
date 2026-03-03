use std::collections::HashMap;

mod helpers;

pub struct Token {
    pub kind: String,
    pub value: String,
}

impl Token {
    pub fn new(kind: &str, value: &str) -> Self {
        Self {
            kind: kind.to_string(),
            value: value.to_string(),
        }
    }

    pub fn is_keyword(&self) -> bool {
        self.kind == "keyword"
    }
}

pub fn parse(input: &str) -> String {
    input.trim().to_string()
}

pub fn tokenize(input: &str) -> Vec<Token> {
    input
        .split_whitespace()
        .map(|w| Token::new("word", w))
        .collect()
}
