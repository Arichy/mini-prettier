use std::collections::HashMap;

use log::debug;

use crate::regexes;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Function,
    Var,
    Let,
    Const,
    // Type,
    Return,
    Equals,          // =
    Eq,              // ==
    NotEq,           // !=
    StrictEq,        // ===
    StrictNotEq,     // !==
    LessThan,        // <
    LessThanOrEq,    // <=
    GreaterThan,     // >
    GreaterThanOrEq, // >=
    Plus,
    Minus,
    Multiply,
    Divide,
    Module,
    Exponent,
    LeftParen,
    RightParen,
    StringLiteral,
    NumericalLiteral,
    Identifier,
    Newline,
    Semicolon,
    Colon,
    Whitespace,
    BOF,
    EOF,
    Unknown,
}

fn create_byte_to_char_map(s: &str) -> HashMap<usize, usize> {
    let mut last_char_index = 0;
    let mut last_byte_index = 0;
    let mut last_char_len = 0;
    let char_count = s.chars().count();
    let mut res = HashMap::new();
    for (char_index, (byte_index, char)) in s.char_indices().enumerate() {
        res.insert(byte_index, char_index);
        if char_index == char_count - 1 {
            last_char_index = char_index;
            last_byte_index = byte_index;
            last_char_len = char.len_utf8();
        }
    }

    res.insert(last_byte_index + last_char_len, last_char_index + 1); // EOF

    res
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub s: &'a str,

    byte_to_char_map: HashMap<usize, usize>,
    char_to_byte_map: HashMap<usize, usize>,

    pub pos: usize,
    pub text: &'a str,
    pub token: Token,
    pub range: (usize, usize),
}

fn char_to_str(c: char, buf: &mut [u8; 4]) -> &str {
    c.encode_utf8(buf);
    std::str::from_utf8(buf).unwrap()
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        let byte_to_char_map = create_byte_to_char_map(s);
        let mut char_to_byte_map = HashMap::new();
        for (byte_index, char_index) in &byte_to_char_map {
            char_to_byte_map.insert(*byte_index, *char_index);
        }

        Lexer {
            s,
            pos: 0,
            byte_to_char_map,
            char_to_byte_map,
            text: "",
            token: Token::BOF,
            range: (0, 0),
        }
    }

    pub fn get_char_pos_by_byte_pos(&self, byte_pos: usize) -> usize {
        *self.byte_to_char_map.get(&byte_pos).unwrap()
    }

    pub fn get_char_pos(&self) -> usize {
        self.get_char_pos_by_byte_pos(self.pos)
    }

    pub fn get_text_char_range(&self) -> (usize, usize) {
        (
            self.get_char_pos_by_byte_pos(self.range.0),
            self.get_char_pos_by_byte_pos(self.range.1),
        )
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
                self.range = (start, self.pos);
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

    pub fn scan_whitespaces(&mut self) {
        self.scan_forward(|c| {
            let mut buf = [0; 4];
            let s: &str = char_to_str(c, &mut buf);

            regexes::white_space().is_match(s)
        });
    }

    pub fn scan(&mut self) {
        self.scan_whitespaces();

        let start = self.pos;
        if self.pos == self.s.len() {
            self.token = Token::EOF;
            return;
        }

        let mut buf = [0; 4];
        let cur_char = self.cur_char();
        let cur_str = char_to_str(cur_char, &mut buf);

        debug!(
            "cur_char: {} at pos:{}, cur_str:{}",
            cur_char, self.pos, cur_str
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
            self.range = (start, self.pos);
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
            self.range = (start, self.pos);

            return;
        }

        match cur_char {
            '+' => {
                self.advance(1);
                self.token = Token::Plus;
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '-' => {
                self.advance(1);
                self.token = Token::Minus;
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '*' => {
                self.advance(1);
                if self.cur_char() == '*' {
                    self.advance(1);
                    self.token = Token::Exponent;
                } else {
                    self.token = Token::Multiply;
                }
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '/' => {
                self.advance(1);
                self.token = Token::Divide;
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '%' => {
                self.advance(1);
                self.token = Token::Module;
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '=' => {
                self.advance(1);
                if self.cur_char() == '=' {
                    self.advance(1);
                    if self.cur_char() == '=' {
                        self.token = Token::StrictEq;
                    } else {
                        self.token = Token::Eq;
                    }
                } else {
                    self.token = Token::Equals;
                }
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '<' => {
                self.advance(1);
                if self.cur_char() == '=' {
                    self.advance(1);
                    self.token = Token::LessThanOrEq;
                } else {
                    self.token = Token::LessThan;
                }
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            '>' => {
                self.advance(1);
                if self.cur_char() == '=' {
                    self.advance(1);
                    self.token = Token::GreaterThanOrEq;
                } else {
                    self.token = Token::GreaterThan;
                }
                self.text = &self.s[start..self.pos];
                self.range = (start, self.pos);
                return;
            }
            _ => {}
        };

        self.token = match cur_char {
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            _ => Token::Unknown,
        };
        self.advance(cur_char.len_utf8());
        self.text = &self.s[start..self.pos];
        self.range = (start, self.pos);
    }

    pub fn lex_all(&mut self) -> Vec<(Token, &str, (usize, usize))> {
        let mut list = vec![];
        while self.token != Token::EOF {
            list.push((self.token, self.text, self.range));
            self.scan();
        }
        list
    }

    pub fn get_string_by_char_range(&self, start: usize, end: usize) -> &str {
        let byte_start = *self.char_to_byte_map.get(&start).unwrap();
        let byte_end = *self.char_to_byte_map.get(&end).unwrap();
        &self.s[byte_start..byte_end]
    }
}

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use super::*;

    #[test]
    fn lexer() {
        let _ = env_logger::try_init();

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
        let _ = env_logger::try_init();

        let s = "
        let a = 1;
        const b = 'hello世界';
        let c = b;
    ";
        let mut lexer = Lexer::new(s);
        let result = lexer.lex_all();

        debug!("{:?}", result);

        assert_eq!(
            result,
            [
                (Token::BOF, "", (0, 0)),
                (Token::Let, "let", ((9, 12))),
                (Token::Identifier, "a", (13, 14)),
                (Token::Equals, "=", (15, 16)),
                (Token::NumericalLiteral, "1", (17, 18)),
                (Token::Semicolon, ";", (18, 19)),
                (Token::Const, "const", (28, 33)),
                (Token::Identifier, "b", (34, 35)),
                (Token::Equals, "=", (36, 37)),
                (Token::StringLiteral, "'hello世界'", (38, 51)),
                (Token::Semicolon, ";", (51, 52)),
                (Token::Let, "let", (61, 64)),
                (Token::Identifier, "c", (65, 66)),
                (Token::Equals, "=", (67, 68)),
                (Token::Identifier, "b", (69, 70)),
                (Token::Semicolon, ";", (70, 71))
            ]
        );
    }
}
