#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_driver;

use tree_sitter_spthy::LANGUAGE;
use tree_sitter;
use tree_sitter::Node;
use std::fs;

fn main() {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Error loading Spthy parser");
    let bytes = fs::read("test.spthy").unwrap();
    let code = fs::read_to_string("test.spthy").unwrap();

    let mut tree = parser.parse(code, None).unwrap();
    let root_node = tree.root_node();
    println!("{:?}", root_node);
    assert!(!root_node.has_error());

    // root
    let mut tc = tree.walk();
    tc.goto_first_child();
    // theory
    println!("{:?}", tc.node());
    tc.goto_next_sibling();
    // theory name
    println!("{:?}", tc.node());
    tc.goto_next_sibling();
}
