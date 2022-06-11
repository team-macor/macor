#![feature(box_syntax)]

pub mod ast;
mod dolev_yao;
pub mod parse;

pub use parse::parse_document;
