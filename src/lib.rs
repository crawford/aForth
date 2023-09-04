// Copyright (C) 2023  Alex Crawford
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::collections::HashMap;
use std::convert::AsRef;
use std::fmt;

pub struct Machine {
    dictionary: HashMap<String, Vec<Token>>,
    stack: Vec<i32>,
}

impl Default for Machine {
    fn default() -> Self {
        use Token::*;
        use Word::*;

        macro_rules! def {
            ($name:literal, $($word:tt),+) => {
                ($name.to_string(), vec![$( def!(@, $word) ),+])
            };
            (@, $val:literal) => {
                Number($val as i32)
            };
            (@, $val:ident) => {
                $val
            };
        }

        let emit = Builtin(Emit);

        Self::with_dictionary(HashMap::from([
            def!("space", ' ', emit),
            def!("cr", '\r', emit, '\n', emit),
        ]))
    }
}

impl Machine {
    fn with_dictionary(dictionary: HashMap<String, Vec<Token>>) -> Self {
        Self {
            dictionary,
            stack: Vec::new(),
        }
    }

    pub fn eval<'a>(&mut self, phrase: &'a str) -> Result<String, Error<'a>> {
        if let Some(def) = phrase.strip_prefix(':') {
            self.eval_def(def).map(|()| String::new())
        } else {
            self.eval_expr(phrase)
        }
    }

    fn eval_def<'a>(&mut self, phrase: &'a str) -> Result<(), Error<'a>> {
        let mut words = phrase.split_ascii_whitespace();
        let name = words
            .next()
            .ok_or(Error::Static("no name specified for definition"))?;

        self.dictionary.insert(name.into(), self.tokenize(words)?);
        Ok(())
    }

    fn eval_expr<'a>(&mut self, phrase: &'a str) -> Result<String, Error<'a>> {
        macro_rules! pop {
            ($op:literal) => {
                self.stack
                    .pop()
                    .ok_or(Error::Static(concat!($op, ": stack underflow")))?
            };
        }

        macro_rules! apply {
            ($name:literal, $op:tt) => {{
                let r = pop!($name) $op pop!($name);
                self.stack.push(r)
            }}
        }

        macro_rules! output {
            ($content:expr, $output:ident) => {
                $output = $output + $content + " "
            };
        }

        self.tokenize(phrase.split_ascii_whitespace())?
            .into_iter()
            .try_fold(String::new(), |mut out, token| -> Result<String, Error> {
                use Token::*;
                use Word::*;

                match token {
                    Builtin(Dot) => output!(&pop!("dot").to_string(), out),
                    Builtin(Emit) => match u32::try_from(pop!("emit")) {
                        Ok(val) => output!(
                            &char::from_u32(val)
                                .ok_or(Error::UnicodeInvalid(val))?
                                .to_string(),
                            out
                        ),
                        _ => return Err(Error::Static("emit: out of bounds")),
                    },
                    Builtin(Plus) => apply!("plus", +),
                    Number(n) => self.stack.push(n),
                }

                Ok(out)
            })
    }

    fn tokenize<'a, I: Iterator<Item = &'a str>>(
        &self,
        strings: I,
    ) -> Result<Vec<Token>, Error<'a>> {
        use Token::*;
        use Word::*;

        let mut tokens = Vec::new();
        for string in strings {
            match string {
                "." => tokens.push(Builtin(Dot)),
                "emit" => tokens.push(Builtin(Emit)),
                "+" => tokens.push(Builtin(Plus)),
                w => match string.parse::<i32>() {
                    Ok(n) => tokens.push(Token::Number(n)),
                    _ => tokens.extend_from_slice(
                        self.dictionary
                            .get(w)
                            .map(AsRef::as_ref)
                            .ok_or(Error::UndefinedWord(w))?,
                    ),
                },
            }
        }
        Ok(tokens)
    }
}

pub enum Error<'a> {
    Static(&'a str),
    UndefinedWord(&'a str),
    UnicodeInvalid(u32),
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match *self {
            Static(err) => f.write_str(err),
            UndefinedWord(w) => write!(f, "undefined word '{w}'"),
            UnicodeInvalid(v) => write!(f, "emit: invalid unicode {v:#04x}"),
        }
    }
}

#[derive(Clone, Copy)]
enum Word {
    Dot,
    Emit,
    Plus,
}

#[derive(Clone, Copy)]
enum Token {
    Builtin(Word),
    Number(i32),
}
