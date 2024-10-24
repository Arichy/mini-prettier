use crate::{generator::generate, parser::parse};

pub fn transform(source: &str) -> String {
    let ast = parse(source);
    let result = generate(ast);

    result
}

#[cfg(test)]
mod tests {
    use std::fs::{self};

    use super::transform;

    #[test]
    fn transform_test() {
        let input = fs::read_to_string("src/__tests__/transform/input.js").unwrap();
        let expected = fs::read_to_string("src/__tests__/transform/output.js").unwrap();
        let output = transform(&input);

        assert_eq!(output, expected);
    }
}
