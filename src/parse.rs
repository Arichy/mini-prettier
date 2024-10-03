use serde::Serialize;

use crate::lexer::{self, Lexer, Token};

#[derive(Debug, Serialize)]
pub struct Location(usize, usize);

#[derive(Debug, Serialize)]
pub struct Identifier<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub name: &'a str,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
pub struct NumericalLiteral {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub value: f64,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
pub struct StringLiteral<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub value: &'a str,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
pub struct AssignmentExpression<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub operator: &'static str,
    pub left: Identifier<'a>,
    pub right: Box<Expression<'a>>,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum BinaryOperator {
    #[serde(rename = "==")]
    Eq,
    #[serde(rename = "!=")]
    NotEq,
    #[serde(rename = "===")]
    StrictEq,
    #[serde(rename = "!==")]
    StrictNotEq,
    #[serde(rename = "<")]
    LessThan,
    #[serde(rename = "<=")]
    LessThanOrEq,
    #[serde(rename = ">")]
    GreaterThan,
    #[serde(rename = ">=")]
    GreaterThanOrEq,
    #[serde(rename = "+")]
    Add,
    #[serde(rename = "-")]
    Subtract,
    #[serde(rename = "*")]
    Multiply,
    #[serde(rename = "/")]
    Divide,
    #[serde(rename = "%")]
    Modulus,
    #[serde(rename = "**")]
    Exponentiation,
}

#[derive(Debug, Serialize)]
pub struct BinaryExpression<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub operator: BinaryOperator,
    left: Box<Expression<'a>>,
    right: Box<Expression<'a>>,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    StringLiteral(StringLiteral<'a>),
    NumericalLiteral(NumericalLiteral),
    AssignmentExpression(AssignmentExpression<'a>),
    BinaryExpression(BinaryExpression<'a>),
}

#[derive(Debug, Serialize)]
pub struct VariableDeclaration<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub kind: &'static str,
    pub declarations: Vec<VariableDeclarator<'a>>,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
pub struct VariableDeclarator<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub id: Identifier<'a>,
    pub init: Option<Expression<'a>>,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Declaration<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Statement<'a> {
    ExpressionStatement(ExpressionStatement<'a>),
    Declaration(Declaration<'a>),
}

#[derive(Debug, Serialize)]
pub struct ExpressionStatement<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub expression: Expression<'a>,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
pub struct Program<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub body: Vec<Statement<'a>>,
    pub pos: Location,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Node<'a> {
    Identifier(Identifier<'a>),
    NumericalLiteral(NumericalLiteral),
    StringLiteral(StringLiteral<'a>),
    AssignmentExpression(AssignmentExpression<'a>),
    Expression(Expression<'a>),
    VariableDeclaration(VariableDeclaration<'a>),
    VariableDeclarator(VariableDeclarator<'a>),
    Declaration(Declaration<'a>),
    Statement(Statement<'a>),
    ExpressionStatement(ExpressionStatement<'a>),
    Program(Program<'a>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Precedence {
    Lowest,
    Assignment,
    Comparison,
    Sum,
    Product,
    Exponentiation,
    Call,
}

impl Precedence {
    fn of(token: &Token) -> Self {
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
    lexer: Lexer<'a>,
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
            type_name: "Program",
            body,
            pos: Location(start, self.lexer.get_char_pos()),
        }
    }

    fn parse_string_literal(&mut self) -> Option<StringLiteral<'a>> {
        if let (true, Some((_, text, range))) = self.try_parse_token(Token::StringLiteral) {
            Some(StringLiteral {
                type_name: "StringLiteral",
                value: text,
                pos: Location(range.0, range.1),
            })
        } else {
            None
        }
    }

    fn parse_numerical_literal(&mut self) -> Option<NumericalLiteral> {
        if let (true, Some((_, text, range))) = self.try_parse_token(Token::NumericalLiteral) {
            Some(NumericalLiteral {
                type_name: "NumericalLiteral",
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
                type_name: "Identifier",
                name: text,
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

            let right = self.parse_expression(Precedence::Assignment, 0)?;
            let start = identifier.pos.0;
            let end = self.get_expression_range(&right).1;
            Some(Expression::AssignmentExpression(AssignmentExpression {
                type_name: "AssignmentExpression",
                left: identifier,
                operator: "=",
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
        let right = self.parse_expression(Precedence::of(&token), 0)?;
        let start = self.get_expression_range(&left).0;
        let end = self.get_expression_range(&right).1;

        Some(Expression::BinaryExpression(BinaryExpression {
            type_name: "BinaryExpression",
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

    fn parse_expression(
        &mut self,
        precedence: Precedence,
        paren_level: usize,
    ) -> Option<Expression<'a>> {
        let mut level = self.parse_left_paren();

        if let Some(mut left) = self.parse_prefix() {
            while let Some(next_precedence) = self.next_precedence() {
                println!(
                    "precedence: {:?}, next_precedence: {:?}, left: {:?}",
                    precedence, next_precedence, left
                );
                if level > 0 {
                } else if precedence >= next_precedence {
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

                while let Token::RightParen = self.lexer.token {
                    if level == 0 {
                        break;
                    }
                    level -= 1;
                    self.lexer.scan();
                }
            }

            Some(left)
        } else {
            None
        }
    }

    fn get_expression_range(&self, expr: &Expression<'a>) -> (usize, usize) {
        match expr {
            Expression::Identifier(i) => (i.pos.0, i.pos.1),
            Expression::AssignmentExpression(a) => (a.pos.0, a.pos.1),
            Expression::NumericalLiteral(n) => (n.pos.0, n.pos.1),
            Expression::StringLiteral(s) => (s.pos.0, s.pos.1),
            Expression::BinaryExpression(b) => (b.pos.0, b.pos.1),
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
                    self.parse_expression(Precedence::Lowest, 0)
                } else {
                    None
                };

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::Declaration(Declaration::VariableDeclaration(
                    VariableDeclaration {
                        type_name: "VariableDeclaration",
                        kind,
                        pos: Location(start, end),
                        declarations: vec![VariableDeclarator {
                            type_name: "VariableDeclarator",
                            id,
                            init,
                            pos: Location(variable_declarator_start, end - 1), // exclude semicolon
                        }],
                    },
                )))
            }
            // Token::Type => {}
            Token::Identifier => {
                let expr = self.parse_expression(Precedence::Lowest, 0)?;

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::ExpressionStatement(ExpressionStatement {
                    type_name: "ExpressionStatement",
                    expression: expr,
                    pos: Location(start, end),
                }))
            }

            Token::StringLiteral => {
                let expr = Expression::StringLiteral(self.parse_string_literal()?);

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::ExpressionStatement(ExpressionStatement {
                    type_name: "ExpressionStatement",
                    expression: expr,
                    pos: Location(start, end),
                }))
            }

            Token::NumericalLiteral => {
                let expr = Expression::NumericalLiteral(self.parse_numerical_literal()?);

                let (_, _, (_, end)) = self.parse_expected(Token::Semicolon);

                Some(Statement::ExpressionStatement(ExpressionStatement {
                    type_name: "ExpressionStatement",
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{read_to_string, write};

    #[test]
    fn parse() {
        let js_code = read_to_string("test.js").unwrap();

        let mut parser = Parser {
            lexer: Lexer::new(&js_code),
        };

        let res = parser.parse();
        let json = serde_json::to_string_pretty(&res).unwrap();
        let _ = write("res.json", json);
        // println!("{}", json);
    }
}
