#[macro_export]
macro_rules! golden_test {
    { expected: $expected:expr, input: $input:expr } => {{
        let expected = $expected.trim();
        let input = format!("{:#?}", $input);
        if expected != input {
            panic!("expected:\n{}\nbut got:\n{}", expected, input);
        }
    }};
}
