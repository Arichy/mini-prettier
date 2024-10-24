use crate::ast::{Program, ToString};

struct Generator<'a> {
    ast: Program<'a>,
}

impl<'a> Generator<'a> {
    pub fn new(ast: Program<'a>) -> Generator<'a> {
        Generator { ast }
    }

    pub fn generate(&self) -> String {
        self.ast.to_string()
    }
}

pub fn generate(ast: Program) -> String {
    let generator = Generator::new(ast);
    generator.generate()
}

mod tests {
    use std::{
        borrow::Cow,
        fs::{read_to_string, write},
    };

    use serde_json::Value;

    use super::Generator;
    use crate::{
        ast::{
            BinaryExpression, Declaration, Expression, ExpressionStatement, Identifier, Location,
            Program, Statement, StringLiteral, VariableDeclaration, VariableDeclarator,
        },
        lexer::Lexer,
        parser::*,
    };

    #[test]
    fn generate_test() {
        let ast = read_to_string("src/__tests__/generate/input.json").unwrap();

        let ast = serde_json::from_str::<Program>(&ast).unwrap();

        let generator = Generator { ast };
        let output = generator.generate();

        let expected = read_to_string("src/__tests__/generate/output.js").unwrap();

        assert_eq!(output, expected);
    }
}
