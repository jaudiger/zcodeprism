pub fn trim_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

pub fn repeat(s: &str, n: usize) -> String {
    s.repeat(n)
}
