use serde::Serialize;

use crate::lexer::{Lexer, Token};

#[derive(Debug, Serialize)]
pub struct Location(usize);

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
#[serde(untagged)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    StringLiteral(StringLiteral<'a>),
    NumericalLiteral(NumericalLiteral),
    AssignmentExpression(AssignmentExpression<'a>),
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
    Sum,
    Product,
    Call,
}

impl Precedence {
    fn of(token: &Token) -> Self {
        match token {
            Token::Equals => Precedence::Assignment,
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

    pub fn parse(&mut self) -> Program<'a> {
        self.lexer.scan();
        self.parse_module()
    }

    fn parse_module(&mut self) -> Program<'a> {
        let mut body = vec![];
        while let Some(statement) = self.parse_statement() {
            body.push(statement);
            self.parse_expected(Token::Semicolon);
        }

        Program {
            type_name: "Program",
            body,
            pos: Location(self.lexer.pos),
        }
    }

    fn parse_string_literal(&mut self) -> Option<StringLiteral<'a>> {
        let text = self.lexer.text;
        if self.try_parse_token(Token::StringLiteral) {
            Some(StringLiteral {
                type_name: "StringLiteral",
                value: text,
                pos: Location(self.lexer.pos),
            })
        } else {
            None
        }
    }

    fn parse_numerical_literal(&mut self) -> Option<NumericalLiteral> {
        let text = self.lexer.text;
        if self.try_parse_token(Token::NumericalLiteral) {
            Some(NumericalLiteral {
                type_name: "NumericalLiteral",
                value: text.parse().unwrap(),
                pos: Location(self.lexer.pos),
            })
        } else {
            None
        }
    }

    fn parse_identifier(&mut self) -> Option<Identifier<'a>> {
        let text = self.lexer.text;
        if self.try_parse_token(Token::Identifier) {
            Some(Identifier {
                type_name: "Identifier",
                name: text,
                pos: Location(self.lexer.pos),
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
            if !self.try_parse_token(Token::Equals) {
                return None;
            }

            let right = self.parse_expression(Precedence::Assignment)?;
            Some(Expression::AssignmentExpression(AssignmentExpression {
                type_name: "AssignmentExpression",
                left: identifier,
                operator: "=",
                right: Box::new(right),
                pos: Location(self.lexer.pos),
            }))
        } else {
            None
        }
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

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression<'a>> {
        if let Some(mut left) = self.parse_prefix() {
            while let Some(next_precedence) = self.next_precedence() {
                println!(
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
        let pos = self.lexer.pos;

        match self.lexer.token {
            Token::Var | Token::Let | Token::Const => {
                let kind = match self.lexer.token {
                    Token::Var => "var",
                    Token::Let => "let",
                    Token::Const => "const",
                    _ => unreachable!(),
                };

                self.lexer.scan();
                let id = self.parse_identifier()?;

                let init = if self.try_parse_token(Token::Equals) {
                    self.parse_expression(Precedence::Lowest)
                } else {
                    None
                };

                Some(Statement::Declaration(Declaration::VariableDeclaration(
                    VariableDeclaration {
                        type_name: "VariableDeclaration",
                        kind,
                        pos: Location(self.lexer.pos),
                        declarations: vec![VariableDeclarator {
                            type_name: "VariableDeclarator",
                            id,
                            init,
                            pos: Location(self.lexer.pos),
                        }],
                    },
                )))
            }
            // Token::Type => {}
            Token::Identifier => Some(Statement::ExpressionStatement(ExpressionStatement {
                type_name: "ExpressionStatement",
                expression: self.parse_expression(Precedence::Lowest)?,
                pos: Location(self.lexer.pos),
            })),

            Token::StringLiteral => Some(Statement::ExpressionStatement(ExpressionStatement {
                type_name: "ExpressionStatement",
                expression: Expression::StringLiteral(self.parse_string_literal()?),
                pos: Location(self.lexer.pos),
            })),

            Token::NumericalLiteral => Some(Statement::ExpressionStatement(ExpressionStatement {
                type_name: "ExpressionStatement",
                expression: Expression::NumericalLiteral(self.parse_numerical_literal()?),
                pos: Location(self.lexer.pos),
            })),
            _ => None,
        }
    }

    fn try_parse_token(&mut self, expected: Token) -> bool {
        let ok = self.lexer.token == expected;

        if ok {
            self.lexer.scan();
        }

        ok
    }

    fn parse_expected(&mut self, expected: Token) {
        if !self.try_parse_token(expected) {
            panic!(
                "{} parse token: Expected {:?}, but got {:?}",
                self.lexer.pos, expected, self.lexer.token
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_to_string;

    #[test]
    fn parse() {
        let js_code = read_to_string("test.js").unwrap();

        let mut parser = Parser {
            lexer: Lexer::new(&js_code),
        };

        let res = parser.parse();
        let json = serde_json::to_string(&res).unwrap();
        println!("{:?}", res);
        println!("{}", json);
    }
}
