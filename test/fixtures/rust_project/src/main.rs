use serde::Deserialize;
use log::info;

#[derive(Deserialize)]
pub struct Config {
    pub name: String,
    pub debug: bool,
}

fn main() {
    info!("starting");
    let _cfg = Config { name: String::new(), debug: false };
}
