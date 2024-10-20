use serde::Serialize;

use crate::{lexer::Token, parser::Precedence};

pub trait ToString {
    fn to_string(&self) -> String;
}

#[derive(Debug, Serialize)]
pub struct Location(pub usize, pub usize);

#[derive(Debug, Serialize)]
pub struct Identifier<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub name: &'a str,
    pub pos: Location,
}
impl<'a> ToString for Identifier<'a> {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

#[derive(Debug, Serialize)]
pub struct NumericalLiteral {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub value: f64,
    pub pos: Location,
}
impl ToString for NumericalLiteral {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Serialize)]
pub struct StringLiteral<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub value: &'a str,
    pub pos: Location,
}
impl<'a> ToString for StringLiteral<'a> {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
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
impl<'a> ToString for AssignmentExpression<'a> {
    fn to_string(&self) -> String {
        format!("{} = {}", self.left.to_string(), self.right.to_string())
    }
}

#[derive(Debug, Serialize, Clone, Copy)]
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

#[derive(Debug, Serialize)]
pub struct BinaryExpression<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
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

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    StringLiteral(StringLiteral<'a>),
    NumericalLiteral(NumericalLiteral),
    AssignmentExpression(AssignmentExpression<'a>),
    BinaryExpression(BinaryExpression<'a>),
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

#[derive(Debug, Serialize)]
pub struct VariableDeclaration<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub kind: &'static str,
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

#[derive(Debug, Serialize)]
pub struct VariableDeclarator<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
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

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Declaration<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
}
impl<'a> ToString for Declaration<'a> {
    fn to_string(&self) -> String {
        match self {
            Declaration::VariableDeclaration(variable_declaration) => {
                variable_declaration.to_string()
            }
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum Statement<'a> {
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

#[derive(Debug, Serialize)]
pub struct ExpressionStatement<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
    pub expression: Expression<'a>,
    pub pos: Location,
}
impl<'a> ToString for ExpressionStatement<'a> {
    fn to_string(&self) -> String {
        format!("{};", self.expression.to_string())
    }
}

#[derive(Debug, Serialize)]
pub struct Program<'a> {
    #[serde(rename(serialize = "type"))]
    pub type_name: &'static str,
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
