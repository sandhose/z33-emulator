enum Node {
    Token(String),
    Punctuation(char),
    Newline,
}

const fn punctuation_eats_space_before(punct: char) -> bool {
    matches!(punct, ',' | ')' | ']')
}

const fn punctuation_eats_space_after(punct: char) -> bool {
    matches!(punct, '(' | '[')
}

impl Node {
    fn emit(&self, next: Option<&Node>) -> String {
        use Node::*;
        match (self, next) {
            // A token before another token yields a space
            (Token(content), Some(Token(_))) => format!("{} ", content),

            // A token before some punctuation yields no space
            (Token(content), Some(Punctuation(punct))) if punctuation_eats_space_before(*punct) => {
                content.clone()
            }

            // A token before some punctuation yields no space
            (Token(content), Some(Punctuation(_))) => format!("{} ", content),

            // A token before a newline yields no extra space
            (Token(content), Some(Newline)) => content.clone(),

            // A token before nothing yields no extra space
            (Token(content), None) => content.clone(),

            // No whitespace after any punctuation before a newline
            (Punctuation(punct), Some(Newline)) => punct.to_string(),

            // No whitespace after any punctuation at the end of the file
            (Punctuation(punct), None) => punct.to_string(),

            // In all other cases, it depends on the type of punctuation
            (Punctuation(punct), _) if punctuation_eats_space_after(*punct) => punct.to_string(),

            (Punctuation(punct), Some(Punctuation(after)))
                if punctuation_eats_space_before(*after) =>
            {
                punct.to_string()
            }

            (Punctuation(punct), _) => format!("{} ", punct),

            // Collapse successive newlines
            (Newline, Some(Newline)) => "".to_string(),

            (Newline, _) => '\n'.to_string(),
        }
    }
}

#[allow(dead_code)]
fn emit_list(list: Vec<Node>) -> String {
    let mut result = String::new();
    let mut iter = list.into_iter().peekable();

    while let Some(item) = iter.next() {
        result += item.emit(iter.peek()).as_str();
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token<T: ToString>(t: T) -> Node {
        Node::Token(t.to_string())
    }
    fn punctuation(c: char) -> Node {
        Node::Punctuation(c)
    }
    fn newline() -> Node {
        Node::Newline
    }

    #[test]
    fn emit_test() {
        let list = vec![
            token("hello"),
            punctuation('('),
            token("world"),
            punctuation(')'),
            punctuation(','),
            token("foo"),
            newline(),
            newline(),
            token("bar"),
            punctuation(','),
            punctuation('['),
            token("%sp"),
            punctuation('+'),
            token("3"),
            punctuation(']'),
            newline(),
        ];

        assert_eq!(
            emit_list(list).as_str(),
            indoc::indoc! {r#"
                hello (world), foo
                bar, [%sp + 3]
            "#}
        );
    }
}
