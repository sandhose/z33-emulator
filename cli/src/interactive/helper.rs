use std::borrow::Cow;
use std::collections::HashSet;
use std::marker::PhantomData;

use ansi_term::Style;
use clap::{App, IntoApp};
use rustyline::{
    completion::Completer,
    highlight::Highlighter,
    hint::Hinter,
    validate::{ValidationContext, ValidationResult, Validator},
    Context,
};
use rustyline_derive::Helper;

/// Rustyline helper, that handles interactive completion, highlighting and hinting.
#[derive(Helper, Debug)]
pub(crate) struct RunHelper<T: IntoApp> {
    app: PhantomData<T>,
}

impl<T: IntoApp> RunHelper<T> {
    pub fn new() -> Self {
        RunHelper { app: PhantomData }
    }
}

fn suggest(app: &App, input: &[String]) -> (usize, HashSet<String>) {
    // We're building the suggestions here
    // The only downside is that it's wasted work if we're not on the first word (second pattern of
    // the match bellow)
    let mut suggestions: HashSet<_> = app
        .get_subcommands()
        .flat_map(|cmd| std::iter::once(cmd.get_name()).chain(cmd.get_visible_aliases()))
        .map(|alias| alias.to_string())
        .collect();

    // If the app has subcommands, it has a `help` command
    if app.has_subcommands() {
        suggestions.insert("help".to_string());
    }

    let mut index = input.len();
    if index > 0 {
        index -= 1
    }

    // Find the curresponding positional arg if it exists and add suggestions for it
    if let Some(arg) = app.get_positionals().nth(index) {
        let additional: Vec<&str> = match arg.get_name() {
            "register" | "address" => vec!["%a", "%b", "%sp", "%sr"],
            _ => Vec::new(),
        };

        suggestions.extend(additional.into_iter().map(String::from))
    }

    match &input[..] {
        [last] => (
            last.len(),
            suggestions
                .into_iter()
                .filter(|alias| alias.starts_with(last))
                .collect(),
        ),

        [head, tail @ ..] => app
            .find_subcommand(head)
            .map(|sub: &App| suggest(sub, tail))
            .unwrap_or_default(),

        [] => (0, suggestions),
    }
}

impl<T: IntoApp> Completer for RunHelper<T> {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let line = &line[..pos];
        let complete = line
            .bytes()
            .last()
            .filter(|&c| c == b' ' || c == b'\t')
            .is_some(); // Line is considered "complete" if the last char is a space
        if let Ok(mut words) = shell_words::split(line) {
            let app = T::into_app();

            // If the last char was a space, insert an empty word to autocomplete the next word
            if complete {
                words.push("".to_string());
            }

            let (offset, candidates) = suggest(&app, words.as_slice());

            Ok((pos - offset, candidates.into_iter().collect()))
        } else {
            Ok((0, Vec::new()))
        }
    }
}

impl<T: IntoApp> Highlighter for RunHelper<T> {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        let style = Style::new().dimmed();
        let hint = style.paint(hint).to_string();
        Cow::Owned(hint)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        let style = Style::new().bold();
        let prompt = style.paint(prompt).to_string();
        Cow::Owned(prompt)
    }
}

impl<T: IntoApp> Hinter for RunHelper<T> {
    type Hint = String;
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        let line = &line[..pos];
        let complete = line
            .bytes()
            .last()
            .filter(|&c| c == b' ' || c == b'\t')
            .is_some(); // Line is considered "complete" if the last char is a space
        let mut words = shell_words::split(line).ok()?;

        // If the last char was a space, insert an empty word to autocomplete the next word
        if complete {
            words.push("".to_string());
        }

        let app = T::into_app();
        let (offset, candidates) = suggest(&app, words.as_slice());

        if candidates.len() == 1 {
            Some(candidates.iter().next().unwrap()[offset..].to_string())
        } else {
            None
        }
    }
}

impl<T: IntoApp> Validator for RunHelper<T> {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        let input = ctx.input();
        let res = shell_words::split(input);
        if res.is_err() {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}
