pub mod helpers;

use std::collections::HashMap;

pub fn build_lookup(keys: &[&str]) -> HashMap<String, usize> {
    let mut map = HashMap::new();
    for (i, k) in keys.iter().enumerate() {
        map.insert(k.to_string(), i);
    }
    map
}
