use nom::Offset;
use std::collections::VecDeque;

const LINES_BEFORE: usize = 2;
const LINES_AFTER: usize = 3;

fn display_line(line: (usize, &str)) {
    let (line, content) = line;
    let line = line + 1;
    eprintln!("{:>5}: {}", line, content);
}

#[allow(dead_code)]
pub fn display_error_offset(input: &str, offset: usize, message: &str) {
    let mut lines = input.lines().enumerate().peekable();

    let mut previous = VecDeque::new();
    let mut current = lines.next().expect("empty program");
    loop {
        let line = lines.peek();
        if let Some((_, l)) = line {
            if ((*l).as_ptr() as usize) - (input.as_ptr() as usize) > offset {
                break;
            }
        } else {
            break;
        }
        previous.push_back(current);
        current = lines.next().unwrap();

        while previous.len() > LINES_BEFORE {
            previous.pop_front();
        }
    }

    let (_, line) = current;
    let line_off = input.offset(line);
    let col = offset - line_off + 1;

    previous.into_iter().for_each(display_line);
    display_line(current);
    eprintln!("       {: >1$}-- {message}", "^", col, message = message);
    lines.take(LINES_AFTER).for_each(display_line);
}
