use tree_sitter_spthy::LANGUAGE;
use tree_sitter;
use std::fs;
use std::collections::HashMap; // Removed HashSet

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
    println!("Filtered Rules: {:?}", rules); // Keep this for now to see filtered rules

    let mut all_facts: Vec<Fact> = Vec::new();
    for rule in &rules {
        all_facts.extend(rule.premises.iter().cloned());
        all_facts.extend(rule.actions.iter().cloned());
        all_facts.extend(rule.conclusions.iter().cloned());
    }

    // Store unique facts by (name, arity) -> Fact. This way we keep one representative Fact.
    let mut unique_facts_map: HashMap<(String, usize), Fact> = HashMap::new();
    for fact in all_facts {
        let signature = (fact.name.clone(), fact.terms.len());
        unique_facts_map.entry(signature).or_insert(fact);
    }

    let unique_relation_declarations: Vec<&Fact> = unique_facts_map.values().collect();

    println!("\nUnique Facts for Relation Declarations (Name, Arity, Example Terms, IsPersistent):");
    for fact in &unique_relation_declarations {
        println!("  - Name: {}, Arity: {}, Terms: {:?}, Persistent: {}", fact.name, fact.terms.len(), fact.terms, fact.is_persistent);
    }

    let ascent_module_string = generate_ascent_module(&unique_relation_declarations);
    println!("\nGenerated Ascent Module:\n{}", ascent_module_string);


    // Read the code again for printing, as `parser.parse` takes ownership
    // let code_for_printing = fs::read_to_string("test.spthy").unwrap();
    // print_tree(&root_node, 0, None, code_for_printing.as_bytes());
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)] // Added PartialEq, Eq, Hash for Fact if it were to be stored directly in HashSet
pub struct Fact {
    pub original_text: String, // Raw text of the fact
    pub name: String,          // Parsed fact name
    pub terms: Vec<String>,    // Parsed terms
    pub is_persistent: bool,
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub name: String,
    pub premises: Vec<Fact>, // These are Vec<Fact>
    pub actions: Vec<Fact>,  // These are Vec<Fact>
    pub conclusions: Vec<Fact>, // These are Vec<Fact>
    // Add a field to store attributes if needed, or parse directly
}

fn extract_rules(root_node: &tree_sitter::Node, source: &[u8]) -> Vec<Rule> {
    let mut rules_vec: Vec<Rule> = Vec::new(); // Renamed to avoid conflict
    for i in 0..root_node.child_count() {
        let node = root_node.child(i).unwrap();
        if node.kind() == "rule" {
            // A "rule" node might be a simple_rule directly or wrapped
            // e.g. rule -> simple_rule
            // or   rule -> diff_rule -> simple_rule (left) & simple_rule (right)
            // For now, we handle direct simple_rule, or simple_rule as first child.
            // A more robust way would be to walk the tree for simple_rule nodes.
            let simple_rule_node_opt = node.child_by_field_name("simple_rule")
                .or_else(|| node.child(0).filter(|c| c.kind() == "simple_rule"));

            if let Some(simple_rule_node) = simple_rule_node_opt {
                // Attempt to extract rule details; this will now filter by role
                if let Some(rule) = extract_rule_details(&simple_rule_node, source) {
                    rules_vec.push(rule);
                }
            }
            // TODO: Handle diff_rule if necessary, which contains multiple simple_rules
        }
    }
    rules_vec
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


fn extract_rule_details(simple_rule_node: &tree_sitter::Node, source: &[u8]) -> Option<Rule> {
    let rule_name_node = simple_rule_node.child_by_field_name("rule_identifier");
    let rule_name = rule_name_node
        .map(|n| n.utf8_text(source).unwrap_or("").to_string())
        .unwrap_or_else(|| "UnknownRule".to_string());

    if rule_name == "UnknownRule" {
        return None;
    }

    let mut is_trustee_role = false;
    // Iterate through named children of simple_rule_node to find "rule_attrs"
    for i in 0..simple_rule_node.named_child_count() {
        let potential_attrs_node = simple_rule_node.named_child(i).unwrap();
        if potential_attrs_node.kind() == "rule_attrs" {
            // Iterate through named children of rule_attrs to find "rule_attr"
            for j in 0..potential_attrs_node.named_child_count() {
                let attr_node = potential_attrs_node.named_child(j).unwrap();
                if attr_node.kind() == "rule_attr" {
                    // rule_attr can have a named child which is the specific attribute type (e.g. rule_role)
                    if let Some(specific_attr_node) = attr_node.named_child(0) {
                        if specific_attr_node.kind() == "rule_role" {
                            if let Some(role_ident_node) = specific_attr_node.child_by_field_name("role_identifier") {
                                let role_text = role_ident_node.utf8_text(source).unwrap_or("");
                                if role_text == "Trustee" {
                                    is_trustee_role = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            if is_trustee_role {
                break;
            }
        }
    }

    if !is_trustee_role {
        return None;
    }

    let mut premises = Vec::new();
    let mut actions = Vec::new();
    let mut conclusions = Vec::new();

    // Iterate through all children to find premise, action_fact, conclusion
    for i in 0..simple_rule_node.child_count() {
        let child = simple_rule_node.child(i).unwrap();
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

fn generate_ascent_module(unique_facts: &[&Fact]) -> String {
    let mut rust_keywords_map: HashMap<String, String> = HashMap::new();
    rust_keywords_map.insert("type".to_string(), "type_".to_string());
    rust_keywords_map.insert("match".to_string(), "match_".to_string());
    // Add other keywords as necessary

    let mut module_content = String::new();
    module_content.push_str("mod generated_relations {\n");
    module_content.push_str("    use ascent::ascent;\n");
    module_content.push_str("\n");
    // TODO: Add type aliases like CfgHash = String; if needed, or infer them.
    // For now, we can manually list some common ones found in src/ascent.rs
    // This part can be made more dynamic or configurable later.
    module_content.push_str("    type CfgHash = String;\n");
    module_content.push_str("    type SkSign = String;\n");
    module_content.push_str("    type SkEncrypt = String;\n");
    module_content.push_str("    type PkSign = String;\n");
    module_content.push_str("    type PkEncrypt = String;\n");
    module_content.push_str("    type NTrustee = u32; // Assuming number, can be String if more general\n");
    module_content.push_str("    type NThreshold = u32; // Assuming number\n");
    module_content.push_str("    type SelfPosition = u32; // Assuming number\n");
    module_content.push_str("    type GenericId = String; // For terms like ~id, %index\n");
    module_content.push_str("\n");

    module_content.push_str("    ascent! {\n");

    for fact in unique_facts {
        let relation_name = rust_keywords_map.get(&fact.name).unwrap_or(&fact.name).to_string();

        let types: Vec<String> = vec!["GenericId".to_string(); fact.terms.len()]; // Default to GenericId (String)
        // A more sophisticated type inference could be added here based on term patterns or a predefined map
        // For example, if fact.name is "Configuration", types might be ["CfgHash", "NTrustee", "NThreshold"]
        // For now, using GenericId for all.

        let type_list = types.join(", ");
        module_content.push_str(&format!("        relation {}({});\n", relation_name, type_list));
    }

    module_content.push_str("    }\n");
    module_content.push_str("}\n");

    module_content
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