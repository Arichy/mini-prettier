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

mod tests {
    use std::fs::{read_to_string, write};

    use super::Generator;
    use crate::{lexer::Lexer, parser::*};

    #[test]
    fn transform() {
        let js_code = read_to_string("test.js").unwrap();

        let mut parser = Parser {
            lexer: Lexer::new(&js_code),
        };

        let ast = parser.parse();

        let generator = Generator { ast };
        let output = generator.generate();
        let _ = write("test_output.js", output);
        // println!("{}", json);
    }
}
