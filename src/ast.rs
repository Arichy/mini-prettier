use std::borrow::{Borrow, Cow};
use std::collections::BTreeMap;

use serde::de::{self, DeserializeOwned, IgnoredAny};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{lexer::Token, parser::Precedence};

pub trait ToString {
    fn to_string(&self) -> String;
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Location(pub usize, pub usize);

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Identifier<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    pub type_name: Cow<'a, str>,
    pub name: Cow<'a, str>,
    pub pos: Location,
}
impl<'a> ToString for Identifier<'a> {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct NumericalLiteral<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    pub type_name: Cow<'a, str>,
    pub value: f64,
    pub pos: Location,
}
impl<'a> ToString for NumericalLiteral<'a> {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct StringLiteral<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    pub type_name: Cow<'a, str>,
    pub value: Cow<'a, str>,
    pub pos: Location,
}
impl<'a> ToString for StringLiteral<'a> {
    fn to_string(&self) -> String {
        let string = self.value.to_string();
        let string = &string[1..string.len() - 1];
        format!("\'{}\'", string)
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct AssignmentExpression<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    #[serde(borrow)]
    pub type_name: Cow<'a, str>,
    pub operator: Cow<'a, str>,
    pub left: Identifier<'a>,
    pub right: Box<Expression<'a>>,
    pub pos: Location,
}
impl<'a> ToString for AssignmentExpression<'a> {
    fn to_string(&self) -> String {
        format!("{} = {}", self.left.to_string(), self.right.to_string())
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
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
impl Into<Token> for BinaryOperator {
    fn into(self) -> Token {
        match self {
            BinaryOperator::Eq => Token::Eq,
            BinaryOperator::NotEq => Token::NotEq,
            BinaryOperator::StrictEq => Token::StrictEq,
            BinaryOperator::StrictNotEq => Token::StrictNotEq,
            BinaryOperator::LessThan => Token::LessThan,
            BinaryOperator::LessThanOrEq => Token::LessThanOrEq,
            BinaryOperator::GreaterThan => Token::GreaterThan,
            BinaryOperator::GreaterThanOrEq => Token::GreaterThanOrEq,
            BinaryOperator::Add => Token::Plus,
            BinaryOperator::Subtract => Token::Minus,
            BinaryOperator::Multiply => Token::Multiply,
            BinaryOperator::Divide => Token::Divide,
            BinaryOperator::Modulus => Token::Module,
            BinaryOperator::Exponentiation => Token::Exponent,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct BinaryExpression<'a> {
    #[serde(borrow)]
    #[serde(rename(serialize = "type", deserialize = "type"))]
    pub type_name: Cow<'a, str>,
    pub operator: BinaryOperator,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
    pub pos: Location,
}
impl<'a> ToString for BinaryExpression<'a> {
    fn to_string(&self) -> String {
        let precedence = Precedence::of(&self.operator.into());
        let need_left_paren =
            if let Expression::BinaryExpression(left_binary_expression) = self.left.as_ref() {
                let left_precedence = Precedence::of(&left_binary_expression.operator.into());
                left_precedence < precedence
            } else {
                false
            };

        let need_right_paren =
            if let Expression::BinaryExpression(right_binary_expression) = self.right.as_ref() {
                let right_precedence = Precedence::of(&right_binary_expression.operator.into());
                right_precedence <= precedence
            } else {
                false
            };

        let left_string = self.left.to_string();
        let left_string = if need_left_paren {
            format!("({})", left_string)
        } else {
            left_string
        };

        let right_string = self.right.to_string();
        let right_string = if need_right_paren {
            format!("({})", right_string)
        } else {
            right_string
        };

        format!(
            "{} {} {}",
            left_string,
            serde_json::to_string(&self.operator)
                .unwrap()
                .trim_matches('"'),
            right_string
        )
    }
}

#[derive(Debug, Serialize, PartialEq)]
#[serde(untagged)]
pub enum Expression<'a> {
    #[serde(borrow)]
    Identifier(Identifier<'a>),
    StringLiteral(StringLiteral<'a>),
    NumericalLiteral(NumericalLiteral<'a>),
    AssignmentExpression(AssignmentExpression<'a>),
    BinaryExpression(BinaryExpression<'a>),
}

impl<'de> Deserialize<'de> for Expression<'_> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor {}

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Expression<'static>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("an expression")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut type_name = None;
                let mut value = None;
                let mut pos = None;
                let mut name = None;
                let mut left = None;
                let mut right = None;
                let mut operator = None;

                while let Ok(Some(key)) = map.next_key::<Value>() {
                    let key = key
                        .as_str()
                        .ok_or(serde::de::Error::custom("Invalid key"))?;
                    match key {
                        "type" => {
                            type_name = Some(map.next_value::<String>()?);
                        }
                        "value" => {
                            value = Some(map.next_value::<serde_json::Value>()?);
                        }
                        "name" => {
                            name = Some(map.next_value::<String>()?);
                        }
                        "pos" => {
                            pos = Some(map.next_value::<Location>()?);
                        }
                        "left" => {
                            left = Some(map.next_value::<serde_json::Value>()?);
                        }
                        "right" => {
                            right = Some(map.next_value::<serde_json::Value>()?);
                        }
                        "operator" => {
                            operator = Some(map.next_value::<serde_json::Value>()?);
                        }
                        _ => {
                            return Err(serde::de::Error::custom(format!(
                                "Unknown field: {:?}",
                                key
                            )));
                        }
                    }
                }

                let type_name = type_name.ok_or(serde::de::Error::custom("Missing type field"))?;
                let pos = pos.ok_or(serde::de::Error::custom("Missing pos field"))?;

                match type_name.as_str() {
                    "Identifier" => {
                        let name = name.ok_or(serde::de::Error::custom("Missing name field"))?;
                        Ok(Expression::Identifier(Identifier {
                            type_name: Cow::Borrowed("Identifier"),
                            name: Cow::Owned(name),
                            pos,
                        }))
                    }
                    "StringLiteral" => {
                        let value = value.ok_or(serde::de::Error::custom("Missing value field"))?;
                        Ok(Expression::StringLiteral(StringLiteral {
                            type_name: Cow::Borrowed("StringLiteral"),
                            value: serde_json::from_value(value)
                                .map_err(serde::de::Error::custom)?,
                            pos,
                        }))
                    }
                    "NumericalLiteral" => {
                        let value = value.ok_or(serde::de::Error::custom("Missing value field"))?;
                        Ok(Expression::NumericalLiteral(NumericalLiteral {
                            type_name: Cow::Borrowed("NumericalLiteral"),
                            value: serde_json::from_value(value)
                                .map_err(serde::de::Error::custom)?,
                            pos,
                        }))
                    }
                    "AssignmentExpression" => {
                        let left_value =
                            left.ok_or(serde::de::Error::custom("Missing left field"))?;
                        let right_value =
                            right.ok_or(serde::de::Error::custom("Missing right field"))?;
                        let operator_value =
                            operator.ok_or(serde::de::Error::custom("Missing operator field"))?;

                        let left =
                            serde_json::from_value(left_value).map_err(serde::de::Error::custom)?;
                        let right: Expression<'_> = serde_json::from_value(right_value)
                            .map_err(serde::de::Error::custom)?;
                        let operator = serde_json::from_value(operator_value)
                            .map_err(serde::de::Error::custom)?;

                        Ok(Expression::AssignmentExpression(AssignmentExpression {
                            type_name: Cow::Borrowed("AssignmentExpression"),
                            operator,
                            left,
                            right: Box::new(right),
                            pos,
                        }))
                    }
                    "BinaryExpression" => {
                        let left_value =
                            left.ok_or(serde::de::Error::custom("Missing left field"))?;
                        let right_value =
                            right.ok_or(serde::de::Error::custom("Missing right field"))?;
                        let operator_value =
                            operator.ok_or(serde::de::Error::custom("Missing operator field"))?;

                        let left =
                            serde_json::from_value(left_value).map_err(serde::de::Error::custom)?;
                        let right = serde_json::from_value(right_value)
                            .map_err(serde::de::Error::custom)?;
                        let operator: BinaryOperator = serde_json::from_value(operator_value)
                            .map_err(serde::de::Error::custom)?;

                        Ok(Expression::BinaryExpression(BinaryExpression {
                            type_name: Cow::Borrowed("BinaryExpression"),
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                            pos,
                        }))
                    }
                    _ => Err(serde::de::Error::custom(format!(
                        "Unknown type: {:?}",
                        type_name
                    ))),
                }
            }
        }

        deserializer.deserialize_map(Visitor {})
    }
}

impl<'a> Expression<'a> {
    pub fn set_pos(&mut self, new_pos: Location) {
        match self {
            Expression::Identifier(identifier) => identifier.pos = new_pos,
            Expression::StringLiteral(string_literal) => string_literal.pos = new_pos,
            Expression::NumericalLiteral(numerical_literal) => numerical_literal.pos = new_pos,
            Expression::AssignmentExpression(assignment_expr) => assignment_expr.pos = new_pos,
            Expression::BinaryExpression(binary_expr) => binary_expr.pos = new_pos,
        }
    }

    pub fn get_range(&self) -> (usize, usize) {
        match self {
            Expression::Identifier(i) => (i.pos.0, i.pos.1),
            Expression::AssignmentExpression(a) => (a.pos.0, a.pos.1),
            Expression::NumericalLiteral(n) => (n.pos.0, n.pos.1),
            Expression::StringLiteral(s) => (s.pos.0, s.pos.1),
            Expression::BinaryExpression(b) => (b.pos.0, b.pos.1),
        }
    }
}

impl<'a> ToString for Expression<'a> {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::StringLiteral(string_literal) => string_literal.to_string(),
            Expression::NumericalLiteral(numerical_literal) => numerical_literal.to_string(),
            Expression::AssignmentExpression(assignment_expr) => assignment_expr.to_string(),
            Expression::BinaryExpression(binary_expr) => binary_expr.to_string(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VariableDeclaration<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    #[serde(borrow)]
    pub type_name: Cow<'a, str>,
    pub kind: Cow<'a, str>,
    pub declarations: Vec<VariableDeclarator<'a>>,
    pub pos: Location,
}
impl<'a> ToString for VariableDeclaration<'a> {
    fn to_string(&self) -> String {
        let declaration_strings = self
            .declarations
            .iter()
            .map(|dec| dec.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{} {}", self.kind, declaration_strings)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VariableDeclarator<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    #[serde(borrow)]
    pub type_name: Cow<'a, str>,
    pub id: Identifier<'a>,
    pub init: Option<Expression<'a>>,
    pub pos: Location,
}
impl<'a> ToString for VariableDeclarator<'a> {
    fn to_string(&self) -> String {
        if let Some(init) = self.init.as_ref() {
            format!("{} = {}", self.id.to_string(), init.to_string())
        } else {
            self.id.to_string()
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Declaration<'a> {
    #[serde(borrow)]
    VariableDeclaration(VariableDeclaration<'a>),
}
impl<'a> ToString for Declaration<'a> {
    fn to_string(&self) -> String {
        match self {
            Declaration::VariableDeclaration(variable_declaration) => {
                format!("{};", variable_declaration.to_string())
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Statement<'a> {
    #[serde(borrow)]
    ExpressionStatement(ExpressionStatement<'a>),
    Declaration(Declaration<'a>),
}
impl<'a> ToString for Statement<'a> {
    fn to_string(&self) -> String {
        match self {
            Self::ExpressionStatement(expression_statement) => expression_statement.to_string(),
            Self::Declaration(declaration) => declaration.to_string(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExpressionStatement<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    #[serde(borrow)]
    pub type_name: Cow<'a, str>,
    pub expression: Expression<'a>,
    pub pos: Location,
}
impl<'a> ToString for ExpressionStatement<'a> {
    fn to_string(&self) -> String {
        format!("{};", self.expression.to_string())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Program<'a> {
    #[serde(rename(serialize = "type", deserialize = "type"))]
    #[serde(borrow)]
    pub type_name: Cow<'a, str>,
    pub body: Vec<Statement<'a>>,
    pub pos: Location,
}
impl<'a> ToString for Program<'a> {
    fn to_string(&self) -> String {
        let strings = self
            .body
            .iter()
            .map(|statement| statement.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        strings
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Node<'a> {
    #[serde(borrow)]
    Identifier(Identifier<'a>),
    NumericalLiteral(NumericalLiteral<'a>),
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

#[cfg(test)]
mod tests {
    use std::{
        borrow::Cow,
        fs::{self},
    };

    use super::*;
    use crate::{
        ast::{BinaryExpression, Expression, Identifier},
        generator::generate,
        parser::parse,
    };

    #[test]
    fn expression_binary_expression_test() {
        let mut b = Expression::BinaryExpression(BinaryExpression {
            type_name: Cow::Borrowed("BinaryExpression"),
            left: Box::new(Expression::Identifier(Identifier {
                type_name: Cow::Borrowed("Identifier"),
                name: Cow::Borrowed("a"),
                pos: Location(0, 1),
            })),
            right: Box::new(Expression::NumericalLiteral(NumericalLiteral {
                type_name: Cow::Borrowed("NumericalLiteral"),
                value: 2.0,
                pos: Location(4, 5),
            })),
            operator: BinaryOperator::Add,
            pos: Location(0, 1),
        });

        let json = serde_json::to_string(&b).unwrap();
        let expr = serde_json::from_str::<Expression>(&json).unwrap();

        assert_eq!(expr, b);
    }

    #[test]
    fn expression_assignment_expression_test() {
        let mut a = Expression::AssignmentExpression(AssignmentExpression {
            type_name: Cow::Borrowed("AssignmentExpression"),
            left: Identifier {
                type_name: Cow::Borrowed("Identifier"),
                name: Cow::Borrowed("a"),
                pos: Location(0, 1),
            },
            right: Box::new(Expression::NumericalLiteral(NumericalLiteral {
                type_name: Cow::Borrowed("NumericalLiteral"),
                value: 2.0,
                pos: Location(4, 5),
            })),
            operator: Cow::Borrowed("="),
            pos: Location(0, 1),
        });

        let json = serde_json::to_string(&a).unwrap();
        let expr = serde_json::from_str::<Expression>(&json).unwrap();

        assert_eq!(expr, a);
    }

    #[test]
    fn expression_identifier_test() {
        let id = Expression::Identifier(Identifier {
            type_name: Cow::Borrowed("Identifier"),
            name: Cow::Borrowed("x"),
            pos: Location(0, 1),
        });

        let json = serde_json::to_string(&id).unwrap();
        let expr = serde_json::from_str::<Expression>(&json).unwrap();

        assert_eq!(expr, id);
    }

    #[test]
    fn expression_string_literal_test() {
        let s = Expression::StringLiteral(StringLiteral {
            type_name: Cow::Borrowed("StringLiteral"),
            value: Cow::Borrowed("\"hello\""),
            pos: Location(0, 5),
        });

        let json = serde_json::to_string(&s).unwrap();
        let expr = serde_json::from_str::<Expression>(&json).unwrap();

        assert_eq!(expr, s);
    }

    #[test]
    fn expression_numerical_literal_test() {
        let n = Expression::NumericalLiteral(NumericalLiteral {
            type_name: Cow::Borrowed("NumericalLiteral"),
            value: 42.0,
            pos: Location(0, 2),
        });

        let json = serde_json::to_string(&n).unwrap();
        let expr = serde_json::from_str::<Expression>(&json).unwrap();

        assert_eq!(expr, n);
    }
}
