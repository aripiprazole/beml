//! This crate provides all the data structures used in the abstract syntax tree,
//! concrete syntax tree, and the intermediate representation.

#![feature(box_patterns)]

pub mod abstr;
pub mod aux;
pub mod concr;
pub mod errors;
pub mod hir;
pub mod loc;
