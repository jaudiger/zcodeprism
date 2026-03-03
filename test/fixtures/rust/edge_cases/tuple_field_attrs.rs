use std::io;

pub enum AppError {
    IoError(#[from] io::Error),
    Custom(#[source] io::Error, String),
    Plain(u32),
}

pub struct Wrapper(#[serde(rename = "inner")] pub String);
