use itertools::Itertools;
use lazy_static::lazy_static;
use std::collections::HashSet;

use crate::errors::InterpreterError;
use crate::interpreter_error;
use crate::token::Token;

lazy_static! {
    static ref QUOTE_CHARS: HashSet<char> = HashSet::from(['"', '\'', '^', '\x16']);
    static ref WHITESPACE_CHARS: HashSet<char> = HashSet::from([' ', '\t', '\n', '\r', '(', ')']);
}

pub struct Tokenizer {
    source: Vec<char>,
    position: usize,
    token_buffer: Vec<char>,
}

impl Tokenizer {
    /// A Tokenizer is constructed with an input string and returns the next available token on request.
    pub fn new(source: &str) -> Tokenizer {
        Tokenizer {
            source: source.chars().collect_vec(),
            position: 0,
            token_buffer: vec![],
        }
    }

    /// Returns the next token in the input string.
    /// If the end of the string is reached, returns Token::EOS.
    /// If an error occurs, returns an InterpreterError.
    ///
    /// # Examples
    ///
    /// ```
    /// use forthic_rs::tokenizer::Tokenizer;
    /// use forthic_rs::token::Token;
    ///
    /// let mut tokenizer = Tokenizer::new("1 2 3");
    /// assert_eq!(tokenizer.next_token().unwrap(), Token::Word("1".to_string()));
    /// assert_eq!(tokenizer.next_token().unwrap(), Token::Word("2".to_string()));
    /// assert_eq!(tokenizer.next_token().unwrap(), Token::Word("3".to_string()));
    /// assert_eq!(tokenizer.next_token().unwrap(), Token::EOS);
    /// ```
    pub fn next_token(&mut self) -> Result<Token, InterpreterError> {
        self.clear_token_buffer();
        self.transition_from_start()
    }

    fn clear_token_buffer(&mut self) {
        self.token_buffer.clear();
    }

    fn flatten_token_buffer(&mut self) -> String {
        self.token_buffer.iter().collect()
    }

    fn is_whitespace(&self, c: &char) -> bool {
        WHITESPACE_CHARS.contains(c)
    }

    fn is_quote(&self, c: &char) -> bool {
        QUOTE_CHARS.contains(c)
    }

    fn is_triple_quote(&mut self, index: usize, c: &char) -> bool {
        if !self.is_quote(c) {
            return false;
        }

        if index + 2 >= self.source.len() {
            return false;
        }

        self.source[index + 1] == *c && self.source[index + 2] == *c
    }

    fn is_start_memo(&mut self, index: usize) -> bool {
        if index + 1 >= self.source.len() {
            return false;
        }

        self.source[index] == '@' && self.source[index + 1] == ':'
    }

    /// Tokenization is implemented as a state machine. This is the entry point.
    fn transition_from_start(&mut self) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;
            if self.is_whitespace(&c) {
                continue;
            } else if c == '#' {
                return self.transition_from_comment();
            } else if c == ':' {
                return self.transition_from_start_definition();
            } else if self.is_start_memo(self.position - 1) {
                // Skip over ":" in "@:"
                self.position += 1;
                return self.transition_from_start_memo();
            } else if c == ';' {
                return Ok(Token::EndDefinition);
            } else if c == '[' {
                return Ok(Token::StartArray);
            } else if c == ']' {
                return Ok(Token::EndArray);
            } else if c == '{' {
                return self.transition_from_gather_module();
            } else if c == '}' {
                return Ok(Token::EndModule);
            } else if self.is_triple_quote(self.position - 1, &c) {
                // Skip over 2nd and 3rd quote chars
                self.position += 2;
                return self.transition_from_gather_triple_quote(c);
            } else if self.is_quote(&c) {
                return self.transition_from_gather_string(c);
            } else {
                // Back up to beginning of word
                self.position -= 1;
                return self.transition_from_gather_word();
            }
        }
        Ok(Token::EOS)
    }

    fn transition_from_comment(&mut self) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.token_buffer.push(c);
            self.position += 1;
            if c == '\n' {
                break;
            }
        }

        Ok(Token::Comment(self.flatten_token_buffer()))
    }

    fn transition_from_start_definition(&mut self) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;

            if self.is_whitespace(&c) {
                continue;
            } else {
                self.position -= 1;
                return self.transition_from_gather_definition_name();
            }
        }

        Err(interpreter_error!("Got EOS in START_DEFINITION"))
    }

    fn transition_from_start_memo(&mut self) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;

            if self.is_whitespace(&c) {
                continue;
            } else {
                self.position -= 1;
                return self.transition_from_gather_memo_name();
            }
        }

        Err(interpreter_error!("Got EOS in START_MEMO"))
    }

    fn gather_definition_name(&mut self) -> Result<(), InterpreterError> {
        let invalid_def_chars = HashSet::from(['[', ']', '{', '}']);

        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;

            if self.is_whitespace(&c) {
                break;
            } else if self.is_quote(&c) {
                return Err(interpreter_error!("Definitions can't have quotes in them"));
            } else if invalid_def_chars.contains(&c) {
                return Err(interpreter_error!(format!(
                    "Definitions can't have {:?} in them",
                    invalid_def_chars
                )));
            } else {
                self.token_buffer.push(c);
            }
        }

        Ok(())
    }

    fn transition_from_gather_definition_name(&mut self) -> Result<Token, InterpreterError> {
        self.gather_definition_name()?;
        Ok(Token::StartDefinition(self.flatten_token_buffer()))
    }

    fn transition_from_gather_memo_name(&mut self) -> Result<Token, InterpreterError> {
        self.gather_definition_name()?;
        Ok(Token::StartMemo(self.flatten_token_buffer()))
    }

    fn transition_from_gather_module(&mut self) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;

            if self.is_whitespace(&c) {
                break;
            } else if c == '}' {
                self.position -= 1;
                break;
            } else {
                self.token_buffer.push(c);
            }
        }

        Ok(Token::StartModule(self.flatten_token_buffer()))
    }

    fn transition_from_gather_triple_quote(
        &mut self,
        delimiter: char,
    ) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];

            if c == delimiter && self.is_triple_quote(self.position, &c) {
                self.position += 3;
                return Ok(Token::String(self.flatten_token_buffer()));
            } else {
                self.position += 1;
                self.token_buffer.push(c);
            }
        }

        Err(interpreter_error!("Unterminated triple quoted string"))
    }

    fn transition_from_gather_string(
        &mut self,
        delimiter: char,
    ) -> Result<Token, InterpreterError> {
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;

            if c == delimiter {
                return Ok(Token::String(self.flatten_token_buffer()));
            } else {
                self.token_buffer.push(c);
            }
        }

        Err(interpreter_error!(format!(
            "Unterminated string ({}), {}",
            delimiter,
            self.flatten_token_buffer()
        )))
    }

    fn transition_from_gather_word(&mut self) -> Result<Token, InterpreterError> {
        let word_delimiters = HashSet::from([';', '[', ']', '}']);
        while self.position < self.source.len() {
            let c = self.source[self.position];
            self.position += 1;

            if self.is_whitespace(&c) {
                break;
            } else if word_delimiters.contains(&c) {
                self.position -= 1;
                break;
            } else {
                self.token_buffer.push(c);
            }
        }

        Ok(Token::Word(self.flatten_token_buffer()))
    }
}

/// Iterator implementation for Tokenizer
impl Iterator for Tokenizer {
    type Item = Result<Token, InterpreterError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::EOS) => None,
            result => Some(result),
        }
    }
}
