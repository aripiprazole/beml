#[cfg_attr(test, macro_export)]
#[allow(unused_macros)]
macro_rules! golden_test {
    { expected: $expected:expr, input: $input:expr } => {{
        let expected = $expected.trim();
        let input = format!("{:#?}", $input);
        if expected != input {
            panic!("expected:\n{}\nbut got:\n{}", expected, input);
        }
    }};
}

#[cfg(test)]
pub use golden_test;
