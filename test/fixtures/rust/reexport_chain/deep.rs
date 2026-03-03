pub struct Widget {
    pub name: String,
}

impl Widget {
    pub fn new() -> Self {
        Self {
            name: String::new(),
        }
    }
}
