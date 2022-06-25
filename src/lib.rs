#![feature(box_syntax, box_patterns, never_type, concat_idents)]

pub mod dolev_yao;
pub mod dolev_yao_old;
pub mod evaluation;
pub mod execution;
pub mod messages;
pub mod protocol;
pub mod typing;
pub mod validate;
pub mod verifier;

pub use macor_parse as parse;
pub use macor_parse::parse_document;

#[cfg(test)]
mod tests {
    macro_rules! test_protocol {
        ( $name:ident, $file:tt, $expects_attack:expr ) => {
            #[test]
            fn $name() {
                let protocol = include_str!($file);
                let result = crate::verifier::Verifier::with_num_sessions(1)
                    .verify(1, &protocol)
                    .unwrap();

                match result {
                    crate::verifier::Verification::Attack(_) => {
                        assert!($expects_attack);
                    }
                    crate::verifier::Verification::NoAttack => {
                        assert!(!$expects_attack);
                    }
                }
            }
        };
    }

    test_protocol!(
        test_protocol_free_pw,
        "../example_programs/FreePW.AnB",
        true
    );
    test_protocol!(
        test_protocol_a_as_b,
        "../example_programs/AasBAttack.AnB",
        true
    );
    test_protocol!(test_simple_sym, "../example_programs/SimpleSym.AnB", true);
    // TODO: This test does not pass
    // test_protocol!(
    //     test_bad_secret_between,
    //     "../example_programs/BadSecretBetween.AnB",
    //     true
    // );
    test_protocol!(test_protocol_ex1, "../example_programs/KeyEx1.AnB", true);
    // TODO: This test does not pass
    // test_protocol!(test_protocol_ex2, "../example_programs/KeyEx2.AnB", true);
    test_protocol!(test_protocol_ex3, "../example_programs/KeyEx3.AnB", false);
    test_protocol!(test_protocol_ex3b, "../example_programs/KeyEx3b.AnB", false);
    test_protocol!(test_protocol_ex4, "../example_programs/KeyEx4.AnB", false);
    test_protocol!(test_protocol_ex4b, "../example_programs/KeyEx4b.AnB", false);
    test_protocol!(test_protocol_ex5, "../example_programs/KeyEx5.AnB", false);
    test_protocol!(test_protocol_ex5b, "../example_programs/KeyEx5b.AnB", false);
    test_protocol!(test_protocol_ex5c, "../example_programs/KeyEx5c.AnB", false);
    test_protocol!(test_protocol_ex6, "../example_programs/KeyEx6.AnB", false);
}
