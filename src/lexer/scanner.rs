#[derive(Debug, Clone)]
pub(super) struct Scanner<'s> {
    input: &'s str,
    pos: usize,
}

impl<'s> Scanner<'s> {
    pub fn new(input: &'s str) -> Scanner<'s> {
        Scanner { input, pos: 0 }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let next = self
            .input
            .chars()
            .enumerate()
            .skip(self.pos)
            .skip_while(|(_, c)| c.is_whitespace())
            .next();

        if let Some((pos, _)) = next {
            self.pos = pos + 1;
        }

        next
    }

    fn peek(&mut self) -> Option<char> {
        if self.pos < self.input.len() {
            self.input.chars().nth(self.pos)
        } else {
            None
        }
    }
}

impl<'s> Iterator for Scanner<'s> {
    type Item = (usize, String);

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos > self.input.len() {
            return None;
        }

        match self.advance() {
            Some(item) => match item {
                (pos, ch) if is_single_symbol(ch) => Some((pos, ch.into())),
                (pos, ch) if is_single_or_double_symbol(ch) => {
                    let peek = self.peek();

                    if peek.is_none() {
                        Some((pos, ch.into()))
                    } else {
                        match (ch, peek.unwrap()) {
                            ('!', '=') => {
                                let _ = self.advance();
                                Some((pos, String::from("!=")))
                            }
                            ('=', '=') => {
                                let _ = self.advance();
                                Some((pos, String::from("!=")))
                            }
                            ('<', '=') => {
                                let _ = self.advance();
                                Some((pos, String::from("<=")))
                            }
                            ('>', '=') => {
                                let _ = self.advance();
                                Some((pos, String::from(">=")))
                            }
                            ('!', _) | ('=', _) | ('>', _) | ('<', _) => Some((pos, ch.into())),
                            _ => unreachable!(),
                        }
                    }
                }
                // TODO take while stream is number
                (start_pos, _) => {
                    let word: String = self
                        .input
                        .chars()
                        .skip(start_pos)
                        .take_while(|c| c.is_alphanumeric() && *c != ';')
                        .map(|c| c.to_string())
                        .collect::<Vec<String>>()
                        .join("");

                    self.pos = start_pos + word.len();

                    Some((start_pos, word))
                }
            },
            None => None,
        }
    }
}

#[inline]
fn is_single_symbol(c: char) -> bool {
    matches!(
        c,
        '{' | '}' | '(' | ')' | '+' | ',' | ';' | '.' | '=' | '-' | '*'
    )
}

#[inline]
fn is_single_or_double_symbol(c: char) -> bool {
    matches!(c, '!' | '=' | '<' | '>')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scanner_breaks_up_symbols() {
        let input = "=+(){},;";

        let expected = vec![
            (0, "=".into()),
            (1, "+".into()),
            (2, "(".into()),
            (3, ")".into()),
            (4, "{".into()),
            (5, "}".into()),
            (6, ",".into()),
            (7, ";".into()),
        ];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_recognizes_extended_symbols() {
        let input = "!-*<5>";

        let expected: Vec<(usize, String)> = input
            .to_string()
            .chars()
            .enumerate()
            .map(|(pos, c)| (pos, c.to_string()))
            .collect();

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_recognizes_keywords() {
        let input = "fn let true false if else return";

        let expected = vec![
            (0, "fn".into()),
            (3, "let".into()),
            (7, "true".into()),
            (12, "false".into()),
            (18, "if".into()),
            (21, "else".into()),
            (26, "return".into()),
        ];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    // #[test]
    // fn scanner_recognizes_multi_char_operators() {
    //     let input = "!= ==";

    //     let expected = vec!["!=", "=="];

    //     let res: Vec<String> = Scanner::new(input).collect();

    //     assert_eq!(expected, res);
    // }

    // #[test]
    // fn scanner_lexes_words() {
    //     let input = "let five_hundred = 500;";

    //     let expected = vec!["let", "five_hundred", "=", "500", ";"];

    //     let res: Vec<String> = Scanner::new(input).collect();

    //     assert_eq!(expected, res);
    // }
}
