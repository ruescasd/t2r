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
    original_text: String, // Raw text of the fact
    name: String,          // Parsed fact name
    terms: Vec<String>,    // Parsed terms
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

// Parses a fact string like "Name(term1, term2)" into ("Name", vec!["term1", "term2"])
// Returns None if parsing fails.
fn parse_fact_string(fact_str: &str) -> Option<(String, Vec<String>)> {
    let fact_str = fact_str.trim();
    let open_paren = fact_str.find('(');
    let close_paren = fact_str.rfind(')');

    if let (Some(op_idx), Some(cp_idx)) = (open_paren, close_paren) {
        if cp_idx != fact_str.len() - 1 { // Ensure ')' is the last character
            return None;
        }

        let name = fact_str[..op_idx].trim().to_string();
        if name.is_empty() {
            return None;
        }

        let terms_str = fact_str[op_idx + 1..cp_idx].trim();
        if terms_str.is_empty() {
            return Some((name, Vec::new())); // Fact with no arguments
        }

        let terms = terms_str.split(',')
            .map(|t| t.trim().to_string())
            .filter(|t| !t.is_empty()) // Handle potential empty strings if there are trailing commas, though grammar might prevent this
            .collect();
        Some((name, terms))
    } else if open_paren.is_none() && close_paren.is_none() {
        // It's a fact with no parentheses, e.g., "MyFact" (nullary)
        let name = fact_str.trim().to_string();
        if name.is_empty() {
            None
        } else {
            Some((name, Vec::new()))
        }
    } else {
        None // Mismatched parentheses or other malformed string
    }
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
                    let original_fact_text = next_child.utf8_text(source).unwrap_or("").trim().to_string();
                    if !original_fact_text.is_empty() {
                        let (name, terms) = parse_fact_string(&original_fact_text)
                            .unwrap_or_else(|| (format!("UNPARSED_FACT: {}", original_fact_text), Vec::new()));
                        facts.push(Fact { original_text: original_fact_text, name, terms, is_persistent: true });
                    }
                    i += 1; // Consume the persistent_fact node
                }
            }
        } else if child.kind() == "persistent_fact" {
            let original_fact_text_full = child.utf8_text(source).unwrap_or("").trim().to_string();
            if !original_fact_text_full.is_empty() {
                let is_persistent_due_to_prefix = original_fact_text_full.starts_with('!');
                let text_to_parse = if is_persistent_due_to_prefix {
                    original_fact_text_full.trim_start_matches('!').trim()
                } else {
                    &original_fact_text_full
                };

                if !text_to_parse.is_empty() {
                    let (name, terms) = parse_fact_string(text_to_parse)
                        .unwrap_or_else(|| (format!("UNPARSED_FACT: {}", text_to_parse), Vec::new()));
                    facts.push(Fact { original_text: original_fact_text_full.clone(), name, terms, is_persistent: true });
                } else if is_persistent_due_to_prefix && text_to_parse.is_empty() { // e.g. just "!" which is unlikely a valid fact
                    facts.push(Fact { original_text: original_fact_text_full.clone(), name: "INVALID_EMPTY_PERSISTENT_FACT".to_string(), terms: Vec::new(), is_persistent: true});
                }
            }
        } else if child.kind() == "linear_fact" {
            let original_fact_text = child.utf8_text(source).unwrap_or("").trim().to_string();
            if !original_fact_text.is_empty() {
                let (name, terms) = parse_fact_string(&original_fact_text)
                    .unwrap_or_else(|| (format!("UNPARSED_FACT: {}", original_fact_text), Vec::new()));
                facts.push(Fact { original_text: original_fact_text, name, terms, is_persistent: false });
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