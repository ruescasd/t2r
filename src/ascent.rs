#![allow(dead_code)]
use ascent::ascent;

type CfgHash = String;
type SkSign = String;
type SkEncrypt = String;
type PkSign = String;
type PkEncrypt = String;
type NTrustee = u32;
type NThreshold = u32;
type SelfPosition = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SignConfiguration(CfgHash);

fn ascent_test() {
    let mut prog = infer::program();
    prog.configuration = vec![
        ("cfg1".to_string(), 3, 2),
    ];
    prog.configuration_trustee = vec![
        ("cfg1".to_string(), 1, "pk_sign_1".to_string(), "pk_encrypt_1".to_string()),
        ("cfg1".to_string(), 2, "pk_sign_2".to_string(), "pk_encrypt_2".to_string()),
        ("cfg1".to_string(), 3, "pk_sign_3".to_string(), "pk_encrypt_3".to_string()),
    ];
    prog.keys = vec![
        ("pk_sign_1".to_string(), "pk_encrypt_1".to_string(), "sk_sign_1".to_string(), "sk_encrypt_1".to_string()),
    ];
    prog.run();
    println!("actions {:?}", prog.action);

    let mut prog2 = execute::program();
    prog2.action = prog.action.clone();
    prog2.position = prog.position.clone();
    prog2.active = vec![(1,), (2,), (3,)]; // Assume all positions
    prog2.run();
    println!("configuration signed: {:?}", prog2.configuration_signed);
}

mod infer {
    use super::*;

        ascent! {
        // Inputs
        relation configuration(CfgHash, NTrustee, NThreshold);
        relation configuration_trustee(CfgHash, SelfPosition, SkSign, SkEncrypt);
        relation keys(PkSign, PkEncrypt, SkSign, SkEncrypt);
        relation configuration_signed(CfgHash, SelfPosition);

        // Inferences
        relation position(CfgHash, SelfPosition);
        relation configuration_signed_up_to(CfgHash, NTrustee);
        
        // Actions
        relation action(SignConfiguration);

        position(cfg_hash, position) <-- 
            configuration_trustee(cfg_hash, position, pk_sign, pk_encrypt),
            keys(pk_sign, pk_encrypt, _, _);

        configuration_signed_up_to(cfg_hash, 1) <-- 
            configuration_signed(cfg_hash, 1);

        configuration_signed_up_to(cfg_hash, n) <-- 
            configuration_signed(cfg_hash, n),
            configuration_signed_up_to(cfg_hash, n - 1);

        action(SignConfiguration(cfg_hash.clone())) <-- 
            configuration(cfg_hash, _, _),
            position(cfg_hash, self_position),
            !configuration_signed(cfg_hash, self_position);

    }

    pub(crate) fn program() -> super::infer::AscentProgram {
        AscentProgram::default()
    }
}

mod execute {
    use super::*;
    
    ascent! {
        // Inputs
        relation active(SelfPosition);
        relation position(CfgHash, SelfPosition);
        relation action(SignConfiguration);
        relation configuration_signed(CfgHash, SelfPosition);

        configuration_signed(cfg_hash, self_position) <-- 
            position(cfg_hash, self_position),
            action(SignConfiguration(cfg_hash.clone())), 
            active(self_position);
    }

    pub(crate) fn program() -> super::execute::AscentProgram {
        AscentProgram::default()
    }
}

#[cfg(test)]
mod tests { 
    use super::*;

    #[test]
    fn test_ascent() {
        ascent_test();
    }
}