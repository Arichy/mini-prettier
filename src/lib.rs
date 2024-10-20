pub mod ast;
pub mod compile;
pub mod generator;
pub mod lexer;
pub mod parser;
mod regexes;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
