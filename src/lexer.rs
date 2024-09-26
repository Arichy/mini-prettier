use std::{iter::Peekable, str::Chars};

use crate::regexes;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Function,
    Var,
    Let,
    Const,
    // Type,
    Return,
    Equals,
    StringLiteral,
    NumericalLiteral,
    Identifier,
    Newline,
    Semicolon,
    Colon,
    Whitespace,
    Unknown,
    BOF,
    EOF,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub s: &'a str,
    // pub chars: Vec<char>,
    // chars: std::str::Chars<'a>,
    pub pos: usize,
    // pub text_range: (usize, usize),
    pub text: &'a str,
    pub token: Token,
}

fn char_to_str(c: char, buf: &mut [u8; 4]) -> &str {
    c.encode_utf8(buf);
    std::str::from_utf8(buf).unwrap()
}

fn char_slice_to_string(chars: &[char]) -> String {
    chars.iter().collect()
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        // let chars = s.chars();

        Lexer {
            s,
            // chars,
            // chars: s.chars().collect(),
            pos: 0,
            // text_range: (0, 0),
            text: "",
            token: Token::BOF,
        }
    }

    #[inline]
    fn assert_at_char_boundray(&self) {
        debug_assert!(self.s.is_char_boundary(self.pos));
    }

    fn cur_char(&self) -> char {
        self.assert_at_char_boundray();
        let string_from_pos = std::str::from_utf8(&self.s.as_bytes()[self.pos..]).unwrap();
        if let Some(char) = string_from_pos.chars().next() {
            char
        } else {
            unsafe { core::hint::unreachable_unchecked() }
        }
    }

    fn cur_byte(&self) -> Option<u8> {
        if self.token == Token::EOF {
            None
        } else {
            Some(self.s.as_bytes()[self.pos])
        }
    }

    fn advance(&mut self, step: usize) {
        self.pos += step;
    }

    pub fn scan_forward<P>(&mut self, pred: P)
    where
        P: Fn(char) -> bool,
    {
        while self.pos < self.s.len() {
            let cur_char = self.cur_char();

            if pred(cur_char) {
                self.advance(cur_char.len_utf8());
            } else {
                break;
            }
        }
    }

    fn lex_string_literal(&mut self, quote: char) {
        let start = self.pos;
        self.advance(1); // skip over the quote

        while self.pos < self.s.len() {
            let cur_char = self.cur_char();
            if cur_char == quote {
                self.advance(1);
                self.token = Token::StringLiteral;
                self.text = &self.s[start..self.pos];
                return;
            }

            if cur_char == '\\' {
                self.advance(1);
                match self.cur_byte() {
                    Some(b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't') => self.advance(1),
                    _ => panic!("Wrong char at pos {}: {}", self.pos, self.cur_char()),
                }
            }

            self.advance(cur_char.len_utf8());
        }
    }

    pub fn scan(&mut self) {
        self.scan_forward(|c| {
            let mut buf = [0; 4];
            let s: &str = char_to_str(c, &mut buf);

            regexes::white_space().is_match(s)
        });

        let start = self.pos;
        if self.pos == self.s.len() {
            self.token = Token::EOF;
            return;
        }

        let mut buf = [0; 4];
        let cur_char = self.cur_char();
        let cur_str = char_to_str(cur_char, &mut buf);

        println!(
            "cur_char: {cur_char} at pos:{}, cur_str:{cur_str}",
            self.pos
        );

        if cur_char == '"' || cur_char == '\'' {
            self.lex_string_literal(cur_char);
            return;
        }

        if regexes::number().is_match(cur_str) {
            self.scan_forward(|c| {
                let mut buf = [0; 4];
                let s = char_to_str(c, &mut buf);
                regexes::number().is_match(s)
            });

            // self.text_range = (start, self.pos);
            self.text = &self.s[start..self.pos];
            self.token = Token::NumericalLiteral;
            return;
        }

        if regexes::alphabet_underscore().is_match(cur_str) {
            self.scan_forward(|c| {
                let mut buf = [0; 4];
                let s = char_to_str(c, &mut buf);
                regexes::alphabet_underscore_number().is_match(s)
            });
            self.text = &self.s[start..self.pos];

            self.token = match self.text {
                "function" => Token::Function,
                "var" => Token::Var,
                "let" => Token::Let,
                "const" => Token::Const,
                // "type" => Token::Type,
                "return" => Token::Return,
                _ => Token::Identifier,
            };

            return;
        }

        self.token = match cur_char {
            '=' => Token::Equals,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            _ => Token::Unknown,
        };
        self.advance(cur_char.len_utf8());
        self.text = &self.s[start..self.pos];
    }

    pub fn lex_all(&mut self) -> Vec<(Token, &str)> {
        let mut list = vec![];
        while self.token != Token::EOF {
            list.push((self.token, self.text));
            self.scan();
        }
        list
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer() {
        let s = "
        var a = 1;
        const b = 'hello世界';
        let c = b;
    ";
        let mut lexer = Lexer::new(s);

        lexer.scan_forward(|c| {
            let mut buf = [0; 4];
            let s = char_to_str(c, &mut buf);
            let res = regexes::white_space().is_match(s);
            res
        });

        let mut cursor = s.find("v").unwrap();
        assert_eq!(lexer.pos, cursor);

        cursor += 3;
        lexer.scan();
        assert_eq!(lexer.pos, cursor);
        assert_eq!(lexer.token, Token::Var);

        cursor += 1 + 1;
        lexer.scan();
        assert_eq!(lexer.pos, cursor);
        assert_eq!(lexer.token, Token::Identifier);
        assert_eq!(lexer.text, "a");

        cursor += 1 + 1;
        lexer.scan();
        assert_eq!(lexer.pos, cursor);
        assert_eq!(lexer.token, Token::Equals);
        assert_eq!(lexer.text, "=");

        cursor += 1 + 1;
        lexer.scan();
        assert_eq!(lexer.pos, cursor);
        assert_eq!(lexer.token, Token::NumericalLiteral);
        assert_eq!(lexer.text, "1");

        cursor += 1;
        lexer.scan();
        assert_eq!(lexer.pos, cursor);
        assert_eq!(lexer.token, Token::Semicolon);
        assert_eq!(lexer.text, ";");

        cursor = s.find("const").unwrap() + 5;
        lexer.scan();
        assert_eq!(lexer.pos, cursor);
        assert_eq!(lexer.token, Token::Const);
        assert_eq!(lexer.text, "const");
    }

    #[test]
    fn lex_all() {
        let s = "
        let a = 1;
        const b = 'hello世界';
        let c = b;
    ";
        let mut lexer = Lexer::new(s);
        let result = lexer.lex_all();

        assert_eq!(
            result,
            [
                (Token::BOF, ""),
                (Token::Let, "let"),
                (Token::Identifier, "a"),
                (Token::Equals, "="),
                (Token::NumericalLiteral, "1"),
                (Token::Semicolon, ";"),
                (Token::Const, "const"),
                (Token::Identifier, "b"),
                (Token::Equals, "="),
                (Token::StringLiteral, "'hello世界'"),
                (Token::Semicolon, ";"),
                (Token::Let, "let"),
                (Token::Identifier, "c"),
                (Token::Equals, "="),
                (Token::Identifier, "b"),
                (Token::Semicolon, ";")
            ]
        );
    }
}
