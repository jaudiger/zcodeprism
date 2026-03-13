// Called by caller(), so this has at least one reference.
pub fn referenced_pub(x: u32) -> u32 {
    x.wrapping_add(1)
}

// Never called by any non-test code.
fn unreferenced_private(x: u32) -> u32 {
    x.wrapping_mul(2)
}

// Public but never called anywhere.
pub fn unreferenced_pub(x: u32) -> u32 {
    x.wrapping_sub(1)
}

// Calls referenced_pub, making it referenced.
pub fn caller() -> u32 {
    referenced_pub(42)
}

struct Counter {
    value: u32,
    limit: u32,
    label: &'static str,
    orphaned: u32,
}

impl Counter {
    fn increment(&self) -> Counter {
        Counter {
            value: self.value + 1,
            limit: self.limit,
            label: self.label,
            orphaned: 0,
        }
    }
}

pub fn use_counter() -> u32 {
    let c = Counter { value: 0, limit: 10, label: "x", orphaned: 0 };
    let c2 = c.increment();
    let _spread = Counter { value: 0, ..c };
    c2.limit
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unreferenced_private() {
        assert_eq!(unreferenced_private(10), 20);
    }
}
