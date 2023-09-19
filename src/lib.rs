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
use std::fmt;

type Tokens = Vec<Token>;

pub struct Machine {
    dictionary: HashMap<String, Vec<Tokens>>,
    stack: Vec<i32>,
}

impl Default for Machine {
    fn default() -> Self {
        macro_rules! def {
            ($name:literal, $( $word:tt ),+) => {
                ($name.to_string(), vec![$( def!(@, $word) ),+])
            };
            (@, $val:literal) => {
                Token::Number($val as i32)
            };
            (@, $val:ident) => {
                Token::Builtin(Word::$val)
            };
        }

        Self::with_dictionary(HashMap::from([
            def!("*", Star),                              // ( n1 n2 -- prod )
            def!("+", Plus),                              // ( n1 n2 -- sum )
            def!("-", Minus),                             // ( n1 n2 -- diff )
            def!(".", Dot),                               // ( n -- )
            def!(".S", StackPrint),                       // ( -- )
            def!("/", Slash),                             // ( n1 n2 -- quot )
            def!("/mod", SlashMod),                       // ( n1 n2 -- quot rem )
            def!("2drop", Drop, Drop),                    // ( d -- )
            def!("2dup", Swap, Dup, Rot, Dup, Rot, Swap), // ( d -- d d )
            def!("2over", TwoOver),                       // ( d1 d2 -- d1 d2 d1 )
            def!("2swap", TwoSwap),                       // ( d1 d2 -- d2 d1 )
            def!("cr", '\r', Emit, '\n', Emit),           // ( -- )
            def!("drop", Drop),                           // ( n -- )
            def!("dup", Dup),                             // ( n -- n n )
            def!("emit", Emit),                           // ( -- )
            def!("mod", Mod),                             // ( n1 n2 -- rem)
            def!("over", Swap, Dup, Rot, Swap),           // ( n1 n2 -- n1 n2 n1 )
            def!("rot", Rot),                             // ( n1 n2 n3 -- n2 n3 n1 )
            def!("space", ' ', Emit),                     // ( -- )
            def!("spaces", Spaces),                       // ( n -- )
            def!("swap", Swap),                           // ( n1 n2 -- n2 n1 )
        ]))
    }
}

impl Machine {
    fn with_dictionary(dictionary: HashMap<String, Vec<Token>>) -> Self {
        Self {
            dictionary: HashMap::from_iter(
                dictionary
                    .into_iter()
                    .map(|(word, tokens)| (word, vec![tokens])),
            ),
            stack: Vec::new(),
        }
    }

    pub fn eval<'a>(&mut self, phrase: &'a str) -> Result<String, Error<'a>> {
        if let Some(def) = phrase.strip_prefix(':') {
            self.eval_def(def).map(|()| String::new())
        } else if let Some(def) = phrase.strip_prefix("forget ") {
            self.eval_undef(def).map(|()| String::new())
        } else {
            self.eval_expr(phrase)
        }
    }

    fn eval_def<'a>(&mut self, phrase: &'a str) -> Result<(), Error<'a>> {
        let mut words = phrase.split_ascii_whitespace();
        let name = words
            .next()
            .ok_or(Error::Static("no name specified for definition"))?;

        let tokens = self.tokenize(words)?;
        self.dictionary.entry(name.into()).or_default().push(tokens);
        Ok(())
    }

    fn eval_undef<'a>(&mut self, word: &'a str) -> Result<(), Error<'a>> {
        let def = self
            .dictionary
            .get_mut(word)
            .ok_or(Error::UndefinedWord(word))?;
        if def.len() == 1 {
            self.dictionary.remove(word);
        } else {
            def.pop();
        }
        Ok(())
    }

    fn eval_expr<'a>(&mut self, phrase: &'a str) -> Result<String, Error<'a>> {
        macro_rules! pop {
            ($op:literal) => {
                pop!($op, 0)
            };
            ($op:literal, $n:literal) => {
                self.stack.remove(
                    self.stack
                        .len()
                        .checked_sub($n + 1)
                        .ok_or(Error::Static(concat!($op, ": stack underflow")))?,
                )
            };
        }

        macro_rules! peek {
            ($op:literal) => {
                peek!($op, 0)
            };
            ($op:literal, $n:literal) => {
                self.stack[self
                    .stack
                    .len()
                    .checked_sub($n + 1)
                    .ok_or(Error::Static(concat!($op, ": stack underflow")))?]
            };
        }

        macro_rules! apply {
            ($name:literal, $op:tt) => {{
                let o = pop!($name);
                let r = pop!($name) $op o;
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
                    Builtin(Drop) => {
                        pop!("drop");
                    }
                    Builtin(Dup) => self.stack.push(peek!("dup")),
                    Builtin(Emit) => match u32::try_from(pop!("emit")) {
                        Ok(val) => output!(
                            &char::from_u32(val)
                                .ok_or(Error::UnicodeInvalid(val))?
                                .to_string(),
                            out
                        ),
                        _ => return Err(Error::Static("emit: out of bounds")),
                    },
                    Builtin(Minus) => apply!("minus", -),
                    Builtin(Mod) => apply!("mod", %),
                    Builtin(Plus) => apply!("plus", +),
                    Builtin(Rot) => {
                        let n = pop!("rot", 2);
                        self.stack.push(n);
                    }
                    Builtin(Slash) => apply!("slash", /),
                    Builtin(SlashMod) => {
                        let b = pop!("slash-mod");
                        let a = pop!("slash-mod");
                        self.stack.push(a % b);
                        self.stack.push(a / b);
                    }
                    Builtin(StackPrint) => {
                        output!(
                            &format!(
                                "<{}> {}",
                                self.stack.len(),
                                self.stack
                                    .iter()
                                    .map(|n| n.to_string())
                                    .collect::<Vec<String>>()
                                    .join(" ")
                            ),
                            out
                        )
                    }
                    Builtin(Spaces) => output!(&" ".repeat(pop!("spaces") as usize), out),
                    Builtin(Star) => apply!("star", *),
                    Builtin(Swap) => {
                        let n = pop!("swap", 1);
                        self.stack.push(n);
                    }
                    Builtin(TwoOver) => {
                        let n1 = peek!("2over", 3);
                        let n2 = peek!("2over", 2);
                        self.stack.push(n1);
                        self.stack.push(n2);
                    }
                    Builtin(TwoSwap) => {
                        let n1 = pop!("2swap", 3);
                        let n2 = pop!("2swap", 2);
                        self.stack.push(n1);
                        self.stack.push(n2);
                    }
                    Number(n) => self.stack.push(n),
                }

                Ok(out)
            })
    }

    fn tokenize<'a, I: Iterator<Item = &'a str>>(
        &self,
        strings: I,
    ) -> Result<Vec<Token>, Error<'a>> {
        let mut comment = false;
        let mut tokens = Vec::new();
        for string in strings {
            match (string, comment) {
                ("(", true) => Err(Error::Static("unbalanced opening comment"))?,
                (")", false) => Err(Error::Static("unbalanced closing comment"))?,
                ("(", false) | (")", true) => {
                    comment = !comment;
                    continue;
                }
                (_, true) => continue,
                _ => {}
            }

            match string.parse::<i32>() {
                Ok(n) => tokens.push(Token::Number(n)),
                _ => tokens.extend_from_slice(
                    self.dictionary
                        .get(string)
                        .and_then(|def| def.last())
                        .ok_or(Error::UndefinedWord(string))?,
                ),
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
enum Token {
    Builtin(Word),
    Number(i32),
}

#[derive(Clone, Copy)]
enum Word {
    Dot,
    Drop,
    Dup,
    Emit,
    Minus,
    Mod,
    Plus,
    Rot,
    Slash,
    SlashMod,
    Spaces,
    StackPrint,
    Star,
    Swap,
    TwoOver,
    TwoSwap,
}
