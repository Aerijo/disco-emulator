use std::io::{Write, stdout};
use crossterm::{screen::{RawScreen, IntoRawMode, EnterAlternateScreen, LeaveAlternateScreen}, Result, execute};

pub fn init() -> Result<RawScreen> {
    execute!(stdout(), EnterAlternateScreen)?;
    let mut stdout = stdout();
    let raw = stdout.into_raw_mode()?;
    return Ok(raw);
}

pub fn close() {
    execute!(stdout(), LeaveAlternateScreen).unwrap();
}
