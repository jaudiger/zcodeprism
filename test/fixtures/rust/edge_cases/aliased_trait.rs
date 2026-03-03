use std::fmt::Display as Disp;

pub struct Point {
    pub x: f64,
    pub y: f64,
}

impl Disp for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}
