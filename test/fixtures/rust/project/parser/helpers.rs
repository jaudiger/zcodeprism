use super::Token;

pub fn is_word_token(t: &Token) -> bool {
    t.kind == "word"
}

pub fn parse_trimmed(input: &str) -> String {
    super::parse(input)
}
