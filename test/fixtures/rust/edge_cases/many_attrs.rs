#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg(target_os = "linux")]
#[allow(dead_code)]
pub struct Annotated {
    #[serde(rename = "_id")]
    pub id: i32,
    pub plain: String,
}

#[inline]
#[must_use]
#[cfg(not(test))]
pub fn heavily_attributed(x: i32) -> i32 {
    x + 1
}

#[derive(Debug)]
pub enum Status {
    #[default]
    Active,
    #[serde(rename = "off")]
    Inactive,
    Plain,
}

#[allow(clippy::cast_possible_truncation)]
/// Converts value with attribute before doc comment.
pub fn attr_before_doc(x: i64) -> i32 {
    x as i32
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
/// A struct with derive and attribute before doc comment.
pub struct DeriveBeforeDoc {
    pub value: i32,
}

#[allow(unused)]
/// A test function with attribute before doc comment.
#[test]
fn test_attr_before_doc() {
    assert_eq!(1, 1);
}

#[repr(C)]
#[derive(Debug, Clone)]
#[serde(tag = "kind")]
pub struct SandwichedDerive {
    pub tag: String,
}

#[macro_export]
macro_rules! exported {
    ($x:expr) => {
        $x + 1
    };
}
