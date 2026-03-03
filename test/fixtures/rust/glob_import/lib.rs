mod utils;
mod sub;

use utils::*;
use utils::inner::*;

pub fn run() -> String {
    helper()
}

pub fn run_other() -> String {
    other()
}

pub fn run_deep() -> String {
    deep_helper()
}
