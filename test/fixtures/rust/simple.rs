//! A simple geometry library for testing.

#![forbid(unsafe_code)]
#![deny(warnings)]

use std::fmt;

#[cfg(not(feature = "unsafe"))]
use std::collections::HashMap;

/// Re-export for convenience.
#[allow(unused_imports)]
use std::io;

/// A point in 2D space.
pub struct Point {
    /// The x coordinate.
    pub x: f64,
    /// The y coordinate.
    pub y: f64,
}

/// Inherent methods for Point.
impl Point {
    /// Create a new point.
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    fn distance(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }

    pub fn manhattan(&self, other: &Point) -> f64 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }

    pub fn is_within_radius(&self, other: &Point, radius: f64) -> bool {
        self.manhattan(other) < radius
    }
}

/// A color enum.
#[derive(Debug, Clone)]
pub enum Color {
    /// Pure red.
    Red,
    /// Pure green.
    Green,
    /// Pure blue.
    Blue,
    Custom(u8, u8, u8),
}

impl Color {
    pub fn is_warm(&self) -> bool {
        matches!(self, Color::Red)
    }
}

/// A drawable trait.
pub trait Drawable {
    type Output;
    fn draw(&self);
    fn bounding_box(&self) -> (f64, f64, f64, f64);
}

/// Drawable implementation for Point.
impl Drawable for Point {
    fn draw(&self) {
        // draw the point
    }

    fn bounding_box(&self) -> (f64, f64, f64, f64) {
        (self.x, self.y, self.x, self.y)
    }
}

/// A generic wrapper type.
pub struct Wrapper<T> {
    pub value: T,
}

impl<T> Wrapper<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Wrapper<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Wrapper({})", self.value)
    }
}

impl<T> From<T> for Wrapper<T> {
    fn from(val: T) -> Self {
        Self { value: val }
    }
}

impl<'a, T> IntoIterator for &'a Wrapper<T> {
    type Item = &'a T;
    type IntoIter = std::iter::Once<&'a T>;
    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(&self.value)
    }
}

pub const MAX_SIZE: usize = 1024;

pub static mut COUNTER: u32 = 0;

pub type Result<T> = std::result::Result<T, String>;

unsafe fn dangerous_operation() -> i32 {
    42
}

async fn fetch_data() -> Vec<u8> {
    vec![]
}

fn helper(x: i32) -> i32 {
    x + 1
}

pub union IntOrFloat {
    i: i32,
    f: f32,
}

macro_rules! say_hello {
    () => {
        println!("Hello!");
    };
}

#[macro_export]
macro_rules! exported_macro {
    ($x:expr) => {
        $x + 1
    };
}

pub mod utils {
    pub fn clamp(val: f64, min: f64, max: f64) -> f64 {
        if val < min {
            min
        } else if val > max {
            max
        } else {
            val
        }
    }
}

#[test]
fn test_point_new() {
    let p = Point::new(1.0, 2.0);
    assert_eq!(p.x, 1.0);
}

extern "C" fn c_callback(x: i32) -> i32 {
    x * 2
}

pub fn uses_point() -> Point {
    let p = Point::new(0.0, 0.0);
    let _ = p.manhattan(&Point::new(1.0, 1.0));
    p
}

pub fn creates_point_literal() -> Point {
    let origin = Point { x: 0.0, y: 0.0 };
    origin
}

pub fn field_access_after_binding() -> f64 {
    let p = Point { x: 1.0, y: 2.0 };
    p.x
}

pub const fn const_add(a: i32, b: i32) -> i32 {
    a + b
}
