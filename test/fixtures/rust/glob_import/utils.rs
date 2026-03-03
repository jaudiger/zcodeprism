pub fn helper() -> String {
    String::from("help")
}

pub fn other() -> String {
    String::from("other")
}

fn private_fn() -> String {
    String::from("private")
}

pub mod inner {
    pub fn deep_helper() -> String {
        String::from("deep")
    }
}
