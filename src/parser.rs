use std::borrow::Cow;

use log::debug;

use crate::ast::*;
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Assignment,
    Comparison,
    Sum,
    Product,
    Exponentiation,
    Call,
}

impl Precedence {
    pub fn of(token: &Token) -> Self {
        match token {
            Token::Equals => Precedence::Assignment,
            Token::Eq
            | Token::NotEq
            | Token::LessThan
            | Token::LessThanOrEq
            | Token::GreaterThan
            | Token::GreaterThanOrEq => Precedence::Comparison,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Multiply | Token::Divide | Token::Module => Precedence::Product,
            Token::Exponent => Precedence::Exponentiation,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn get_token(&self) -> Token {
        self.lexer.token
    }

    pub fn parse(&mut self) -> Program<'a> {
        self.parse_module()
    }

    fn parse_module(&mut self) -> Program<'a> {
        let mut body = vec![];
        let start = self.lexer.get_char_pos();

        while let Some(statement) = self.parse_statement() {
            body.push(statement);
        }

        Program {
            type_name: Cow::Borrowed("Program"),
            body,
            pos: Location(start, self.lexer.get_char_pos()),
        }
    }

    fn parse_string_literal(&mut self) -> Option<StringLiteral<'a>> {
        if let (true, Some((_, text, range))) = self.try_parse_token(Token::StringLiteral) {
            Some(StringLiteral {
                type_name: Cow::Borrowed("StringLiteral"),
                value: Cow::Borrowed(text),
                pos: Location(range.0, range.1),
            })
        } else {
            None
        }
    }

    fn parse_numerical_literal(&mut self) -> Option<NumericalLiteral<'a>> {
        if let (true, Some((_, text, range))) = self.try_parse_token(Token::NumericalLiteral) {
            Some(NumericalLiteral {
                type_name: Cow::Borrowed("NumericalLiteral"),
                value: text.parse().unwrap(),
                pos: Location(range.0, range.1),
            })
        } else {
            None
        }
    }

    fn parse_identifier(&mut self) -> Option<Identifier<'a>> {
        if let (true, Some((_, text, range))) = self.try_parse_token(Token::Identifier) {
            Some(Identifier {
                type_name: Cow::Borrowed("Identifier"),
                name: Cow::Borrowed(text),
                pos: Location(range.0, range.1),
            })
        } else {
            None
        }
    }

    fn is_valid_assignment_target(expr: Expression) -> bool {
        matches!(expr, Expression::Identifier(_))
    }

    fn parse_assignment_expression(&mut self, left: Expression<'a>) -> Option<Expression<'a>> {
        if let Expression::Identifier(identifier) = left {
            if !self.try_parse_token(Token::Equals).0 {
                return None;
            }

            let right = self.parse_expression(Precedence::Assignment)?;
            let start = identifier.pos.0;
            let end = right.get_range().1;
            Some(Expression::AssignmentExpression(AssignmentExpression {
                type_name: Cow::Borrowed("AssignmentExpression"),
                left: identifier,
                operator: Cow::Borrowed("="),
                right: Box::new(right),
                pos: Location(start, end),
            }))
        } else {
            None
        }
    }

    fn parse_binary_expression(&mut self, left: Expression<'a>) -> Option<Expression<'a>> {
        let token = self.lexer.token;
        self.lexer.scan();
        let right = self.parse_expression(Precedence::of(&token))?;
        let start = left.get_range().0;
        let end = right.get_range().1;

        Some(Expression::BinaryExpression(BinaryExpression {
            type_name: Cow::Borrowed("BinaryExpression"),
            left: Box::new(left),
            operator: match token {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                Token::Multiply => BinaryOperator::Multiply,
                Token::Divide => BinaryOperator::Divide,
                Token::Exponent => BinaryOperator::Exponentiation,
                Token::Module => BinaryOperator::Modulus,
                Token::Eq => BinaryOperator::Eq,
                Token::NotEq => BinaryOperator::NotEq,
                Token::LessThan => BinaryOperator::LessThan,
                Token::LessThanOrEq => BinaryOperator::LessThanOrEq,
                Token::GreaterThan => BinaryOperator::GreaterThan,
                Token::GreaterThanOrEq => BinaryOperator::GreaterThanOrEq,
                _ => unreachable!(),
            },
            right: Box::new(right),
            pos: Location(start, end),
        }))
    }

    fn next_precedence(&self) -> Option<Precedence> {
        if self.lexer.token == Token::EOF {
            None
        } else {
            Some(Precedence::of(&self.lexer.token))
        }
    }

    fn parse_prefix(&mut self) -> Option<Expression<'a>> {
        if self.get_token() == Token::LeftParen {
            self.lexer.scan();
            let result = self.parse_expression(Precedence::Lowest);

            self.parse_expected(Token::RightParen);
            // result.as_ref().unwrap().

            return result;
        }

        match &self.lexer.token {
            Token::Identifier => Some(Expression::Identifier(self.parse_identifier()?)),
            Token::StringLiteral => Some(Expression::StringLiteral(self.parse_string_literal()?)),
            Token::NumericalLiteral => Some(Expression::NumericalLiteral(
                self.parse_numerical_literal()?,
            )),
            _ => None,
        }
    }

    fn parse_left_paren(&mut self) -> usize {
        let mut level = 0;
        while let Token::LeftParen = self.lexer.token {
            level += 1;
            self.lexer.scan();
        }

        level
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression<'a>> {
        debug!("Start parsing expression, pos: {}", self.lexer.pos);
        if let Some(mut left) = self.parse_prefix() {
            while let Some(next_precedence) = self.next_precedence() {
                debug!(
                    "precedence: {:?}, next_precedence: {:?}, left: {:?}",
                    precedence, next_precedence, left
                );

                if precedence >= next_precedence {
                    break;
                }

                match &self.lexer.token {
                    Token::Equals => {
                        left = self.parse_assignment_expression(left)?;
                    }
                    Token::Plus
                    | Token::Minus
                    | Token::Multiply
                    | Token::Divide
                    | Token::Exponent
                    | Token::Module
                    | Token::Eq
                    | Token::NotEq
                    | Token::LessThan
                    | Token::LessThanOrEq
                    | Token::GreaterThan
                    | Token::GreaterThanOrEq => {
                        left = self.parse_binary_expression(left)?;
                    }
                    _ => {
                        break;
                    }
                }
            }

            Some(left)
        } else {
            None
        }
    }

    fn parse_statement(&mut self) -> Option<Statement<'a>> {
        if self.lexer.token == Token::BOF {
            self.lexer.scan();
        }

        let start = self.lexer.get_text_char_range().0;

        match self.lexer.token {
            Token::Var | Token::Let | Token::Const => {
                let kind = match self.lexer.token {
                    Token::Var => "var",
                    Token::Let => "let",
                    Token::Const => "const",
                    _ => unreachable!(),
                };

                self.lexer.scan();
                let variable_declarator_start = self.lexer.get_text_char_range().0;
                let id = self.parse_identifier()?;

                let init = if self.try_parse_token(Token::Equals).0 {
                    self.parse_expression(Precedence::Lowest)
                } else {
                    None
                };

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::Declaration(Declaration::VariableDeclaration(
                    VariableDeclaration {
                        type_name: Cow::Borrowed("VariableDeclaration"),
                        kind: Cow::Borrowed(kind),
                        pos: Location(start, end),
                        declarations: vec![VariableDeclarator {
                            type_name:Cow::Borrowed("VariableDeclarator"),
                            id,
                            init,
                            pos: Location(variable_declarator_start, end - 1), // exclude semicolon
                        }],
                    },
                )))
            }
            // Token::Type => {}
            Token::Identifier => {
                let expr = self.parse_expression(Precedence::Lowest)?;

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::ExpressionStatement(ExpressionStatement {
                    type_name: Cow::Borrowed("ExpressionStatement"),
                    expression: expr,
                    pos: Location(start, end),
                }))
            }

            Token::StringLiteral => {
                let expr = Expression::StringLiteral(self.parse_string_literal()?);

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::ExpressionStatement(ExpressionStatement {
                    type_name: Cow::Borrowed("ExpressionStatement"),
                    expression: expr,
                    pos: Location(start, end),
                }))
            }

            Token::NumericalLiteral => {
                let expr = Expression::NumericalLiteral(self.parse_numerical_literal()?);

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::ExpressionStatement(ExpressionStatement {
                    type_name: Cow::Borrowed("ExpressionStatement"),
                    expression: expr,
                    pos: Location(start, end),
                }))
            }
            _ => None,
        }
    }

    fn try_parse_token(
        &mut self,
        expected: Token,
    ) -> (bool, Option<(Token, &'a str, (usize, usize))>) {
        let ok = self.lexer.token == expected;

        if ok {
            let token = self.lexer.token;
            let text = self.lexer.text;
            let range = self.lexer.get_text_char_range();
            self.lexer.scan();
            (true, Some((token, text, range)))
        } else {
            (false, None)
        }
    }

    fn parse_expected(&mut self, expected: Token) -> (Token, &'a str, (usize, usize)) {
        let res = self.try_parse_token(expected);
        if !res.0 {
            panic!(
                "{} parse token: Expected {:?}, but got {:?}",
                self.lexer.get_char_pos(),
                expected,
                self.lexer.token
            );
        }
        res.1.unwrap()
    }
}

pub fn parse(source: &str) -> Program {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use serde_json::{to_value, Value};

    use super::*;
    use std::fs::{read_to_string, write};

    #[test]
    fn parse() {
        let input = read_to_string("src/__tests__/parse/input.js").unwrap();

        let mut parser = Parser {
            lexer: Lexer::new(&input),
        };

        let ast = parser.parse();
        let output = to_value(&ast).unwrap();
        let expected = read_to_string("src/__tests__/parse/output.json").unwrap();
        let expected: Value = serde_json::from_str(&expected).unwrap();

        assert_eq!(output, expected);
    }
}
