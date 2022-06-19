#![feature(box_syntax, box_patterns, never_type)]

pub mod dolev_yao;
pub mod messages;
pub mod protocol;
pub mod typing;

pub use macor_parse as parse;
pub use macor_parse::parse_document;
