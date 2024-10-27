#[derive(Debug, Clone)]
pub struct Scanner<'s> {
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
            .skip_while(|(_, c)| *c == ' ' || *c == '\t')
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

    fn take_word(&mut self, start: usize) -> String {
        self.take(start, |c| (c.is_alphanumeric() || *c == '_') && *c != ';')
    }

    fn take_number(&mut self, start: usize) -> String {
        self.take(start, |c| (c.is_numeric() || *c == '.') && *c != ';')
    }

    fn skip_line(&mut self, start: usize) {
        let next = self
            .input
            .chars()
            .enumerate()
            .skip(start)
            .skip_while(|(_, c)| *c != '\n')
            .next();

        if let Some((pos, _)) = next {
            self.pos = pos;
        }
    }

    fn skip_multi_line_comment(&mut self, start: usize) {
        let mut iter = self.input.chars().enumerate().skip(start).peekable();

        while let Some((_, ch)) = iter.next() {
            if ch == '*' {
                if let Some((pos, peek)) = iter.peek() {
                    if *peek == '/' {
                        self.pos = *pos + 1;
                        break;
                    }
                }
            }
        }
    }

    fn take<P>(&mut self, start_pos: usize, predicate: P) -> String
    where
        P: FnMut(&char) -> bool,
    {
        self.input
            .chars()
            .skip(start_pos)
            .take_while(predicate)
            .map(|c| c.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

impl<'s> Iterator for Scanner<'s> {
    type Item = (usize, String);

    fn next(&mut self) -> Option<Self::Item> {
        fn is_double_symbol(left: char, right: char) -> bool {
            match (left, right) {
                ('!', '=') | ('=', '=') | ('<', '=') | ('>', '=') | ('\r', '\n') => true,
                _ => false,
            }
        }

        if self.pos > self.input.len() {
            return None;
        }

        let next_match = match self.advance() {
            Some(item) => match item {
                (pos, '"') => {
                    let string = self.take(pos + 1, |c| *c != '"');
                    Some((pos, String::from_iter(["\"".into(), string, "\"".into()])))
                }
                (_, '\r') | (_, '\n') => self.next(),
                (pos, ch) if is_single_or_double_symbol(ch) => match self.peek() {
                    Some(peek) => match (ch, peek) {
                        (left, right) if is_double_symbol(left, right) => {
                            Some((pos, String::from_iter([ch, peek])))
                        }
                        // single-line comments
                        ('/', '/') => {
                            self.skip_line(pos);
                            self.next()
                        }
                        // multi-line comments
                        ('/', '*') => {
                            self.skip_multi_line_comment(pos);
                            self.next()
                        }
                        (ch, _) => Some((pos, ch.into())),
                    },
                    None => Some((pos, ch.into())),
                },
                (pos, ch) if is_single_symbol(ch) => Some((pos, ch.into())),
                (start_pos, ch) if ch.is_numeric() => {
                    Some((start_pos, self.take_number(start_pos)))
                }

                (start_pos, ch) if ch.is_alphabetic() => {
                    Some((start_pos, self.take_word(start_pos)))
                }
                (pos, ch) => Some((pos, ch.into())), // the lexer will discard this token if it is some invalid character
            },
            None => None,
        };

        if let Some((pos, word)) = next_match.clone() {
            // messy
            if word != "//" {
                self.pos = pos + word.len();
            }
        }

        next_match
    }
}

#[inline]
fn is_single_symbol(c: char) -> bool {
    matches!(
        c,
        '{' | '}' | '(' | ')' | '+' | ',' | ';' | '.' | '=' | '-' | '*' | '\n' | '/'
    )
}

#[inline]
fn is_single_or_double_symbol(c: char) -> bool {
    matches!(c, '!' | '=' | '<' | '>' | '\r' | '/')
}

#[cfg(test)]
mod scanner_tests {
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

    #[test]
    fn scanner_recognizes_multi_char_operators() {
        let input = "!= == <= >= < foo >";

        let expected = vec![
            (0, "!=".into()),
            (3, "==".into()),
            (6, "<=".into()),
            (9, ">=".into()),
            (12, "<".into()),
            (14, "foo".into()),
            (18, ">".into()),
        ];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_lexes_words() {
        let input = "var five_hundred = 500.1234;";

        let expected = vec![
            (0, "var".into()),
            (4, "five_hundred".into()),
            (17, "=".into()),
            (19, "500.1234".into()),
            (27, ";".into()),
        ];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_breaks_newlines() {
        let input = "foo\nbar\r\n\n\nbaz";
        let expected = vec![(0, "foo".into()), (4, "bar".into()), (11, "baz".into())];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_ingests_slashes_and_not_comments() {
        let input = "// this is a comment\nfoobar/bazquux";

        let expected = vec![
            (21, "foobar".into()),
            (27, "/".into()),
            (28, "bazquux".into()),
        ];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_takes_strings() {
        let input = r#"foo = "this is a string literal""#;

        let expected = vec![
            (0, "foo".into()),
            (4, "=".into()),
            (6, r#""this is a string literal""#.into()),
        ];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }

    #[test]
    fn scanner_skips_multi_line_comments() {
        let input = r#"/* this is a multi-line
comment and
I can put anything here: foo + 1 2 3 */
1 + foo"#;

        let expected = vec![(76, "1".into()), (78, "+".into()), (80, "foo".into())];

        let res: Vec<(usize, String)> = Scanner::new(input).collect();

        assert_eq!(expected, res);
    }
}
