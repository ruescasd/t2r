use tree_sitter_spthy::LANGUAGE;
use tree_sitter;
use std::fs;

fn main() {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Error loading Spthy parser");
    let code = fs::read_to_string("test.spthy").unwrap();

    let tree = parser.parse(code, None).unwrap();
    let root_node = tree.root_node();
    // assert!(!root_node.has_error()); // TODO: Temporarily removed to inspect parsing errors

    let source_code = fs::read_to_string("test.spthy").unwrap();
    let rules = extract_rules(&root_node, source_code.as_bytes());
    println!("{:?}", rules);

    // Read the code again for printing, as `parser.parse` takes ownership
    // let code_for_printing = fs::read_to_string("test.spthy").unwrap();
    // print_tree(&root_node, 0, None, code_for_printing.as_bytes());
}

#[derive(Debug, Clone)]
struct Fact {
    text: String, // For now, just store the raw text. Can be parsed further later.
    is_persistent: bool,
}

#[derive(Debug, Clone)]
struct Rule {
    name: String,
    premises: Vec<Fact>,
    actions: Vec<Fact>,
    conclusions: Vec<Fact>,
}

fn extract_rules(root_node: &tree_sitter::Node, source: &[u8]) -> Vec<Rule> {
    let mut rules: Vec<Rule> = Vec::new();
    for i in 0..root_node.child_count() {
        let node = root_node.child(i).unwrap();
        if node.kind() == "rule" {
            let simple_rule_node_opt = node.child_by_field_name("simple_rule")
                .or_else(|| node.child(0).filter(|c| c.kind() == "simple_rule"));

            if let Some(simple_rule_node) = simple_rule_node_opt {
                if let Some(rule) = extract_rule_details(&simple_rule_node, source) {
                    rules.push(rule);
                }
            }
        }
    }
    rules
}

fn extract_rule_details(rule_node: &tree_sitter::Node, source: &[u8]) -> Option<Rule> {
    let rule_name = rule_node
        .child_by_field_name("rule_identifier")
        .map(|n| n.utf8_text(source).unwrap_or("").to_string())
        .unwrap_or_else(|| "UnknownRule".to_string());

    if rule_name == "UnknownRule" { // Should not happen if grammar is correct for rules
        return None;
    }

    let mut premises = Vec::new();
    let mut actions = Vec::new();
    let mut conclusions = Vec::new();

    for i in 0..rule_node.child_count() {
        let child = rule_node.child(i).unwrap();
        match child.kind() {
            "premise" => {
                premises.extend(extract_facts(&child, source));
            }
            "action_fact" => {
                actions.extend(extract_facts(&child, source));
            }
            "conclusion" => {
                conclusions.extend(extract_facts(&child, source));
            }
            _ => {}
        }
    }
    Some(Rule { name: rule_name, premises, actions, conclusions })
}

fn extract_facts(parent_node: &tree_sitter::Node, source: &[u8]) -> Vec<Fact> {
    let mut facts = Vec::new();
    let mut i = 0;
    while i < parent_node.child_count() {
        let child = parent_node.child(i).unwrap();

        if child.kind() == "!" {
            if i + 1 < parent_node.child_count() {
                let next_child = parent_node.child(i + 1).unwrap();
                if next_child.kind() == "persistent_fact" {
                    let fact_text = next_child.utf8_text(source).unwrap_or("").trim().to_string();
                    if !fact_text.is_empty() {
                        facts.push(Fact { text: fact_text, is_persistent: true });
                    }
                    i += 1; // Consume the persistent_fact node
                }
            }
        } else if child.kind() == "persistent_fact" {
            // Handles cases like '--[ !Fact ]->' where 'persistent_fact' node includes '!'
            let fact_text_full = child.utf8_text(source).unwrap_or("").trim().to_string();
            if !fact_text_full.is_empty() {
                // Check if it actually starts with '!' as part of its own text
                let is_persistent = fact_text_full.starts_with('!');
                let text_to_store = if is_persistent {
                    fact_text_full.trim_start_matches('!').trim().to_string()
                } else {
                    fact_text_full
                };
                 if !text_to_store.is_empty() {
                    facts.push(Fact { text: text_to_store, is_persistent });
                }
            }
        } else if child.kind() == "linear_fact" {
            let fact_text = child.utf8_text(source).unwrap_or("").trim().to_string();
            if !fact_text.is_empty() {
                facts.push(Fact { text: fact_text, is_persistent: false });
            }
        }
        i += 1;
    }
    facts
}


fn print_tree(node: &tree_sitter::Node, level: usize, field_name: Option<&'static str>, source: &[u8]) {
    let indent = "  ".repeat(level);
    let field_name_str = field_name.map_or_else(|| "".to_string(), |name| format!("({}): ", name));
    println!(
        "{}{}{} [{} - {}] '{}'",
        indent,
        field_name_str,
        node.kind(),
        node.start_position(),
        node.end_position(),
        node.utf8_text(source).unwrap_or("").lines().next().unwrap_or("").trim()
    );

    let mut cursor = node.walk();
    for i in 0..node.child_count() {
        let child = node.child(i).unwrap();
        let child_field_name = node.field_name_for_child(i as u32);
        print_tree(&child, level + 1, child_field_name, source);
    }
}