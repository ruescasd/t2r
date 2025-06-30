use tree_sitter_spthy::LANGUAGE;
use tree_sitter;
use std::fs;
use std::collections::{HashMap, HashSet}; // Added HashSet back

fn main() {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Error loading Spthy parser");
    let code = fs::read_to_string("test.spthy").unwrap();

    let tree = parser.parse(code, None).unwrap();
    let root_node = tree.root_node();
    // assert!(!root_node.has_error()); // TODO: Temporarily removed to inspect parsing errors

    // Blacklist will be defined later, before the transformation step.
    // For now, extract_rules parses everything.
    let source_code = fs::read_to_string("test.spthy").unwrap();
    let parsed_rules = extract_rules(&root_node, source_code.as_bytes());
    println!("Parsed Rules (Unfiltered): {:?}", parsed_rules);

    // Define the blacklist (as per next step, but needed for the call)
    let mut fact_blacklist: HashSet<String> = HashSet::new();
    fact_blacklist.insert("Unique".to_string());

    // Step 2: Implement Transformation Function (called in Step 3 of plan)
    let transformed_rules = transform_rules_for_ascent(parsed_rules, &fact_blacklist);
    println!("\nTransformed Rules (e.g., 'Unique' filtered): {:?}", transformed_rules);

    // --- Data Collection from Transformed Rules ---

    // Unique AscentFacts for relation declarations: (name, arity) -> Vec<original_term_strings>
    // We only care about the structure for declaration, not is_negated here.
    let mut unique_ascent_fact_signatures: HashMap<(String, usize), Vec<String>> = HashMap::new();

    // Unique AscentEffects for struct generation: (struct_name, arity) -> Vec<original_term_strings>
    let mut unique_ascent_effect_signatures: HashMap<(String, usize), Vec<String>> = HashMap::new();

    let mut unique_term_names: HashSet<String> = HashSet::new();

    for ascent_rule in &transformed_rules {
        for ascent_fact in &ascent_rule.premises {
            unique_ascent_fact_signatures
                .entry((ascent_fact.name.clone(), ascent_fact.terms.len()))
                .or_insert_with(|| ascent_fact.terms.clone());
            for term in &ascent_fact.terms {
                unique_term_names.insert(term.clone());
            }
        }
        for ascent_fact in &ascent_rule.conclusions {
            unique_ascent_fact_signatures
                .entry((ascent_fact.name.clone(), ascent_fact.terms.len()))
                .or_insert_with(|| ascent_fact.terms.clone());
            for term in &ascent_fact.terms {
                unique_term_names.insert(term.clone());
            }
        }
        for ascent_effect in &ascent_rule.effects {
            unique_ascent_effect_signatures
                .entry((ascent_effect.struct_name.clone(), ascent_effect.terms.len()))
                .or_insert_with(|| ascent_effect.terms.clone());
            for term in &ascent_effect.terms {
                unique_term_names.insert(term.clone());
            }
        }
    }

    println!("\nUnique AscentFact Signatures for Relations (Name, Arity, ExampleTerms):");
    for ((name, arity), terms) in &unique_ascent_fact_signatures {
        println!("  - Name: {}, Arity: {}, Terms: {:?}", name, arity, terms);
    }

    println!("\nUnique AscentEffect Signatures for Structs (StructName, Arity, ExampleTerms):");
    for ((name, arity), terms) in &unique_ascent_effect_signatures {
        println!("  - StructName: {}, Arity: {}, Terms: {:?}", name, arity, terms);
    }

    println!("\nUnique Term Names (from Ascent structures): {:?}", unique_term_names);

    // generate_ascent_module will need to be updated significantly in Phase 3.
    // For now, let's pass the new collections. It will likely fail or produce partial output.
    // We'll create placeholder structs from unique_ascent_fact_signatures to pass to the old generate_ascent_module signature for now.
    // Remove temporary shim for generate_ascent_module call
    let ascent_module_string = generate_ascent_module(
        &unique_ascent_fact_signatures,
        &unique_ascent_effect_signatures,
        &unique_term_names,
        &transformed_rules // Add transformed_rules to the call
    );
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

// --- Structs for Ascent Transformation ---
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AscentFact {
    pub name: String,          // Name of the fact, e.g., "ConfigurationSigned" (after stripping "Not")
    pub terms: Vec<String>,    // Original term strings, e.g., ["cfg_id", "%1"]
    pub is_negated: bool,      // True if this fact is negated in a premise
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AscentEffect {
    pub name: String,          // Original full name, e.g., "EffectSignConfiguration"
    pub struct_name: String,   // Name of the struct to be generated, e.g., "SignConfiguration"
    pub terms: Vec<String>,    // Original term strings
}

#[derive(Debug, Clone)]
pub struct AscentRule {
    pub name: String, // Original Tamarin rule name
    pub premises: Vec<AscentFact>,
    pub conclusions: Vec<AscentFact>,
    pub effects: Vec<AscentEffect>,
}
// --- End Structs for Ascent Transformation ---

// Reverted extract_rules signature
fn extract_rules(root_node: &tree_sitter::Node, source: &[u8]) -> Vec<Rule> {
    let mut rules_vec: Vec<Rule> = Vec::new();
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
                // No blacklist passed here anymore
                if let Some(rule) = extract_rule_details(&simple_rule_node, source) {
                    rules_vec.push(rule);
                }
            }
            // TODO: Handle diff_rule if necessary, which contains multiple simple_rules
        }
    }
    rules_vec
}

// Note: `parsed_rules` is moved into this function.
fn transform_rules_for_ascent(parsed_rules: Vec<Rule>, fact_blacklist: &HashSet<String>) -> Vec<AscentRule> {
    let mut transformed_ascent_rules = Vec::new();

    for parsed_rule in parsed_rules {
        let mut current_ascent_premises: Vec<AscentFact> = Vec::new();
        let mut current_ascent_conclusions: Vec<AscentFact> = Vec::new();
        let mut current_ascent_effects: Vec<AscentEffect> = Vec::new();

        // Process Tamarin Premises
        for fact in &parsed_rule.premises {
            if fact_blacklist.contains(&fact.name) {
                continue;
            }
            let (ascent_fact_name, is_negated) = if fact.name.starts_with("Not") {
                (fact.name["Not".len()..].to_string(), true)
            } else {
                (fact.name.clone(), false)
            };
            current_ascent_premises.push(AscentFact {
                name: ascent_fact_name,
                terms: fact.terms.clone(),
                is_negated,
            });
        }

        // Process Tamarin Actions (as Ascent Premises)
        for fact in &parsed_rule.actions {
            if fact_blacklist.contains(&fact.name) {
                continue;
            }
            let (ascent_fact_name, is_negated) = if fact.name.starts_with("Not") {
                (fact.name["Not".len()..].to_string(), true)
            } else {
                (fact.name.clone(), false)
            };
            current_ascent_premises.push(AscentFact { // Add to premises
                name: ascent_fact_name,
                terms: fact.terms.clone(),
                is_negated,
            });
        }

        // Process Tamarin Conclusions (as Ascent Conclusions or Effects)
        for fact in &parsed_rule.conclusions {
            if fact_blacklist.contains(&fact.name) {
                continue;
            }
            if fact.name.starts_with("Effect") {
                let struct_name = fact.name["Effect".len()..].to_string();
                // A more robust way to get struct_name would be to_camel_case on the remainder,
                // but for now, direct stripping is used as per EffectSignConfiguration -> SignConfiguration example.
                // We might need to ensure struct_name is a valid CamelCase identifier later.
                // For now, assume EffectXyz will result in Xyz.
                current_ascent_effects.push(AscentEffect {
                    name: fact.name.clone(), // Original name "EffectSomething"
                    struct_name,             // "Something"
                    terms: fact.terms.clone(),
                });
            } else {
                // Regular conclusion
                current_ascent_conclusions.push(AscentFact {
                    name: fact.name.clone(), // Conclusion names are used as is
                    terms: fact.terms.clone(),
                    is_negated: false, // Conclusions are typically not negated
                });
            }
        }

        let ascent_rule = AscentRule {
            name: parsed_rule.name, // Name is preserved
            premises: current_ascent_premises,
            conclusions: current_ascent_conclusions,
            effects: current_ascent_effects,
        };
        transformed_ascent_rules.push(ascent_rule);
    }
    transformed_ascent_rules
}

// Helper to check for patterns that should directly map to "Number" in relation signatures
// and do NOT need a separate type alias like "type T1 = Number;" to be generated for them by generate_type_aliases.
// Terms like %index *will* get an alias "type Index = Number;", so is_direct_number_pattern("%index") is false.
fn is_direct_number_pattern(term: &str) -> bool {
    if term.starts_with('%') {
        let inner = &term[1..];
        if inner.is_empty() { return false; } // Just "%" is not a direct number pattern

        // Case 1: % followed by only digits (e.g., %1, %23)
        if inner.chars().all(|c| c.is_digit(10)) {
            return true;
        }
        // Case 2: % followed by a single alphabetic character (e.g., %i, %n)
        // These will be 'Number' directly in relations, and no 'type I = Number;' will be generated by generate_type_aliases.
        if inner.chars().count() == 1 && inner.chars().next().unwrap().is_alphabetic() {
            return true;
        }
        // Case 3: Contains operators or another '%' indicating an expression (e.g. %i %+ %1)
        if inner.contains('+') || inner.contains('-') || inner.contains('*') || inner.contains('/') || inner.contains('%') {
            return true;
        }
    }
    false
}

// Gets the string representation for a term's type to be used in a relation signature.
// This could be "Number" directly, or a CamelCase alias like "CfgId" or "Index".
fn get_term_type_representation(term_original_str: &str) -> String {
    if is_direct_number_pattern(term_original_str) {
        "Number".to_string()
    } else {
        // It's a symbolic term, generate its CamelCase alias name.
        // This name (e.g., CfgId, Index, T1 if %1 was not caught by direct_number_pattern)
        // is expected to have a corresponding `type AliasName = BaseType;` definition.
        let mut alias_name = to_camel_case(term_original_str);

        // Ensure T-prefix for names starting with digit (e.g. if to_camel_case("%0foo") -> "0Foo" then "T0Foo")
        // This is important if a term like "%0" was NOT caught by is_direct_number_pattern
        // (currently it would be, but as a safeguard for `to_camel_case` general use).
        if let Some(first_char) = alias_name.chars().next() {
            if first_char.is_digit(10) {
                alias_name = format!("T{}", alias_name);
            }
        }

        // Fallback for safety if alias_name ended up empty or as "UnnamedTerm"
        if alias_name.is_empty() || alias_name == "UnnamedTerm" {
            // This case should ideally not be hit if to_camel_case is robust
            // and unique_term_names are sensible.
            // If the original term looked numeric-like, default its type rep to Number, else Generic.
            if term_original_str.starts_with('%') {
                "Number".to_string()
            } else {
                "Generic".to_string()
            }
        } else {
            alias_name
        }
    }
}

fn to_camel_case(term_name: &str) -> String {
    let mut s = term_name.trim();

    if s.starts_with('%') || s.starts_with('~') {
        s = &s[1..];
    }

    if s.is_empty() {
        return "UnnamedTerm".to_string(); // Or handle as an error/option
    }

    let mut result = String::new();
    let mut capitalize_next = true;

    for c in s.chars() {
        if c == '_' || c == ' ' || c == '+' {
            capitalize_next = true;
        } else if c.is_alphanumeric() {
            if capitalize_next {
                result.push(c.to_ascii_uppercase());
                capitalize_next = false;
            } else {
                result.push(c);
            }
        }
        // Other characters (like internal '%', multiple '+', etc.) are skipped
    }

    // Ensure first char is indeed uppercase if the string wasn't empty after stripping prefix and processing
    if !result.is_empty() {
        let mut chars = result.chars();
        if let Some(first_char) = chars.next() {
            if first_char.is_lowercase() { // Should not happen if logic above is correct for non-empty s
                let rest = chars.as_str();
                return format!("{}{}", first_char.to_ascii_uppercase(), rest);
            }
        }
    }


    // If the result is empty (e.g. term was just "%" or "_"), return a default.
    if result.is_empty() {
        "UnnamedTerm".to_string()
    } else {
        result
    }
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

// Reverted extract_rule_details signature
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
                premises.extend(extract_facts(&child, source)); // Reverted: No blacklist
            }
            "action_fact" => {
                actions.extend(extract_facts(&child, source)); // Reverted: No blacklist
            }
            "conclusion" => {
                conclusions.extend(extract_facts(&child, source)); // Reverted: No blacklist
            }
            _ => {}
        }
    }

    Some(Rule { name: rule_name, premises, actions, conclusions })
}

// Reverted extract_facts signature and logic (no blacklist check)
fn extract_facts(parent_node: &tree_sitter::Node, source: &[u8]) -> Vec<Fact> {
    let mut facts = Vec::new();
    let mut i = 0;
    while i < parent_node.child_count() {
        let child = parent_node.child(i).unwrap();
        // let mut fact_to_add: Option<Fact> = None; // No longer needed with direct push

        if child.kind() == "!" {
            if i + 1 < parent_node.child_count() {
                let next_child = parent_node.child(i + 1).unwrap();
                if next_child.kind() == "persistent_fact" {
                    let original_fact_text = next_child.utf8_text(source).unwrap_or("").trim().to_string();
                    if !original_fact_text.is_empty() {
                        let (name, terms) = parse_fact_string(&original_fact_text)
                            .unwrap_or_else(|| (format!("UNPARSED_FACT: {}", original_fact_text), Vec::new()));
                        // No blacklist check, directly add
                        facts.push(Fact { original_text: original_fact_text, name, terms, is_persistent: true });
                    }
                    i += 1;
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
                    // No blacklist check, directly add
                    facts.push(Fact { original_text: original_fact_text_full.clone(), name, terms, is_persistent: true });
                } else if is_persistent_due_to_prefix && text_to_parse.is_empty() {
                     let name = "INVALID_EMPTY_PERSISTENT_FACT".to_string();
                    // No blacklist check, directly add
                    facts.push(Fact { original_text: original_fact_text_full.clone(), name, terms: Vec::new(), is_persistent: true});
                }
            }
        } else if child.kind() == "linear_fact" {
            let original_fact_text = child.utf8_text(source).unwrap_or("").trim().to_string();
            if !original_fact_text.is_empty() {
                 let (name, terms) = parse_fact_string(&original_fact_text)
                    .unwrap_or_else(|| (format!("UNPARSED_FACT: {}", original_fact_text), Vec::new()));
                // No blacklist check, directly add
                facts.push(Fact { original_text: original_fact_text, name, terms, is_persistent: false });
            }
        }
        // No fact_to_add option, directly pushed if parsed.
        i += 1;
    }
    facts
}

fn generate_ascent_module(
    unique_ascent_fact_signatures: &HashMap<(String, usize), Vec<String>>,
    unique_ascent_effect_signatures: &HashMap<(String, usize), Vec<String>>,
    unique_term_names: &HashSet<String>,
    transformed_rules: &[AscentRule] // Add new parameter
) -> String {
    let mut rust_keywords_map: HashMap<String, String> = HashMap::new();
    rust_keywords_map.insert("type".to_string(), "type_".to_string());
    rust_keywords_map.insert("match".to_string(), "match_".to_string());
    // Add other keywords as necessary

    let mut module_content = String::new();
    module_content.push_str("mod generated_relations {\n");
    module_content.push_str("    use ascent::ascent;\n");
    module_content.push_str("\n");

    module_content.push_str(&generate_type_aliases(unique_term_names));
    module_content.push_str("\n");

    // Implement Effect Struct Generation
    if !unique_ascent_effect_signatures.is_empty() {
        module_content.push_str("    // ---- Effect Struct Definitions ----\n");
        // Sort for consistent output order
        let mut sorted_effect_signatures: Vec<_> = unique_ascent_effect_signatures.iter().collect();
        sorted_effect_signatures.sort_by_key(|((name, _arity), _terms)| name.clone());

        for ((struct_name, _arity), terms) in sorted_effect_signatures {
            let field_types: Vec<String> = terms.iter()
                .map(|term_original_str| get_term_type_representation(term_original_str))
                .collect();

            let struct_def = format!(
                "    #[derive(Debug, Clone, PartialEq, Eq, Hash)]\n    pub struct {}({});\n",
                struct_name, // This is already the StructName, e.g., "SignConfiguration"
                field_types.join(", ")
            );
            module_content.push_str(&struct_def);
        }
        module_content.push_str("    // ---- End Effect Struct Definitions ----\n\n");
    }

    module_content.push_str("    ascent! {\n");

    // Generate Standard Relations
    if !unique_ascent_fact_signatures.is_empty() {
        module_content.push_str("        // ---- Standard Relations ----\n");
        let mut sorted_fact_relations: Vec<_> = unique_ascent_fact_signatures.iter().collect();
        sorted_fact_relations.sort_by_key(|((name, _arity), _terms)| name.clone());
        for ((name, _arity), terms) in sorted_fact_relations {
            let relation_name = rust_keywords_map.get(name).unwrap_or(name).to_string();
            let param_types: Vec<String> = terms.iter()
                .map(|term_original_str| get_term_type_representation(term_original_str))
                .collect();
            module_content.push_str(&format!("        relation {}({});\n", relation_name, param_types.join(", ")));
        }
        module_content.push_str("        // ---- End Standard Relations ----\n\n");
    }

    // Generate Effect Relations
    if !unique_ascent_effect_signatures.is_empty() {
        module_content.push_str("        // ---- Effect Relations ----\n");
        let mut sorted_effect_relations: Vec<_> = unique_ascent_effect_signatures.iter().collect();
        sorted_effect_relations.sort_by_key(|((struct_name, _arity), _terms)| struct_name.clone());
        for ((struct_name, _arity), _terms) in sorted_effect_relations {
            module_content.push_str(&format!("        relation effect_{}({});\n", struct_name, struct_name));
        }
        module_content.push_str("        // ---- End Effect Relations ----\n\n");
    }

    // Generate Ascent Rule Bodies
    if !transformed_rules.is_empty() {
        module_content.push_str("        // ---- Ascent Rules ----\n");
        for ascent_rule in transformed_rules {
            let premises_str: Vec<String> = ascent_rule.premises.iter().map(|p_fact| {
                let terms_str: Vec<String> = p_fact.terms.iter()
                    .map(|t| format_tamarin_term_for_ascent_rule(t))
                    .collect();
                let negation_str = if p_fact.is_negated { "!" } else { "" };
                format!("{}{}({})", negation_str, p_fact.name, terms_str.join(", "))
            }).collect();

            let body_str = premises_str.join(", ");

            for conclusion_fact in &ascent_rule.conclusions {
                let head_terms_str: Vec<String> = conclusion_fact.terms.iter()
                    .map(|t| format_tamarin_term_for_ascent_rule(t))
                    .collect();
                let head_str = format!("{}({})", conclusion_fact.name, head_terms_str.join(", "));
                if body_str.is_empty() {
                    module_content.push_str(&format!("        {}.\n", head_str));
                } else {
                    module_content.push_str(&format!("        {} <-- {};\n", head_str, body_str));
                }
            }

            for effect in &ascent_rule.effects {
                let effect_terms_str: Vec<String> = effect.terms.iter()
                    .map(|t| format_tamarin_term_for_ascent_rule(t))
                    .collect();
                // Head: effect_StructName(StructName(term1, term2, ...))
                let head_str = format!("effect_{}({}({}))",
                                       effect.struct_name,
                                       effect.struct_name,
                                       effect_terms_str.join(", "));
                if body_str.is_empty() {
                    module_content.push_str(&format!("        {}.\n", head_str));
                } else {
                    module_content.push_str(&format!("        {} <-- {};\n", head_str, body_str));
                }
            }
        }
        module_content.push_str("        // ---- End Ascent Rules ----\n");
    }

    module_content.push_str("    }\n");
    module_content.push_str("}\n");

    module_content
}

fn generate_type_aliases(term_names: &HashSet<String>) -> String {
    let mut type_aliases_str = String::new();
    type_aliases_str.push_str("    // ---- Base Types ----\n");
    type_aliases_str.push_str("    type Generic = String;\n");
    type_aliases_str.push_str("    type Number = u32;\n");
    type_aliases_str.push_str("    // ---- Generated Term Aliases ----\n");

    let mut sorted_term_names: Vec<String> = term_names.iter().cloned().collect();
    sorted_term_names.sort(); // Sort for consistent output order

    for term_name_original in sorted_term_names {
        // If the term is a direct number pattern (e.g. %1, %i, %i %+ %1),
        // it will be represented as "Number" directly in relations and does not need its own alias.
        if is_direct_number_pattern(&term_name_original) {
            continue;
        }

        let mut alias_name = to_camel_case(&term_name_original);

        if let Some(first_char) = alias_name.chars().next() {
            if first_char.is_digit(10) {
                alias_name = format!("T{}", alias_name);
            }
        }

        if alias_name == "Generic" || alias_name == "Number" || alias_name.is_empty() || alias_name == "UnnamedTerm" {
            continue;
        }

        // For remaining terms (symbolic ones like %index, cfg_id):
        // Determine base type based on whether the *original* symbolic term started with '%'
        let mapped_rust_type = if term_name_original.starts_with('%') {
            "Number" // e.g. %index -> type Index = Number;
        } else {
            "Generic" // e.g. cfg_id -> type CfgId = Generic;
        };
        type_aliases_str.push_str(&format!("    type {} = {};\n", alias_name, mapped_rust_type));
    }
    type_aliases_str.push_str("    // ---- End Generated Term Aliases ----\n");
    type_aliases_str
}

fn format_tamarin_term_for_ascent_rule(term_str: &str) -> String {
    let mut s = term_str.to_string();

    if s.starts_with('~') {
        s = s[1..].to_string();
    }

    s = s.replace("%", "");

    // Normalize whitespace to handle cases like "i  +  1" becoming "i + 1"
    // and also trims leading/trailing whitespace that might have been around '%'
    s.split_whitespace().collect::<Vec<&str>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_tamarin_term_for_ascent_rule() {
        assert_eq!(format_tamarin_term_for_ascent_rule("%1"), "1");
        assert_eq!(format_tamarin_term_for_ascent_rule("%i"), "i");
        assert_eq!(format_tamarin_term_for_ascent_rule("%self_index"), "self_index");
        assert_eq!(format_tamarin_term_for_ascent_rule("cfg_id"), "cfg_id");
        assert_eq!(format_tamarin_term_for_ascent_rule("~id"), "id");
        assert_eq!(format_tamarin_term_for_ascent_rule("%i %+ %1"), "i + 1");
        assert_eq!(format_tamarin_term_for_ascent_rule("%i%+%1"), "i+1"); // Test without spaces around operator
        assert_eq!(format_tamarin_term_for_ascent_rule("  %spaced_out_term  "), "spaced_out_term");
        assert_eq!(format_tamarin_term_for_ascent_rule("%term %with %spaces"), "term with spaces");
    }
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