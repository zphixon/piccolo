use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Clone, Copy, Debug)]
pub enum Color {
    Reset = 0,
    None = 1,
    Black = 30,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    White = 37,
    BrightBlack = 90,
    BrightRed = 91,
    BrightGreen = 92,
    BrightYellow = 93,
    BrightBlue = 94,
    BrightMagenta = 95,
    BrightCyan = 96,
    BrightWhite = 97,
}

impl Color {
    pub fn fg(self) -> Option<&'static str> {
        use Color::*;
        match self {
            Reset => Option::None,
            None => Some(""),
            Black => Some("\u{1b}[30m"),
            Red => Some("\u{1b}[31m"),
            Green => Some("\u{1b}[32m"),
            Yellow => Some("\u{1b}[33m"),
            Blue => Some("\u{1b}[34m"),
            Magenta => Some("\u{1b}[35m"),
            Cyan => Some("\u{1b}[36m"),
            White => Some("\u{1b}[37m"),
            BrightBlack => Some("\u{1b}[90m"),
            BrightRed => Some("\u{1b}[91m"),
            BrightGreen => Some("\u{1b}[92m"),
            BrightYellow => Some("\u{1b}[93m"),
            BrightBlue => Some("\u{1b}[94m"),
            BrightMagenta => Some("\u{1b}[95m"),
            BrightCyan => Some("\u{1b}[96m"),
            BrightWhite => Some("\u{1b}[97m"),
        }
    }

    pub fn bg(self) -> Option<&'static str> {
        use Color::*;
        match self {
            Reset => Option::None,
            None => Some(""),
            Black => Some("\u{1b}[40m"),
            Red => Some("\u{1b}[41m"),
            Green => Some("\u{1b}[42m"),
            Yellow => Some("\u{1b}[43m"),
            Blue => Some("\u{1b}[44m"),
            Magenta => Some("\u{1b}[45m"),
            Cyan => Some("\u{1b}[46m"),
            White => Some("\u{1b}[47m"),
            BrightBlack => Some("\u{1b}[100m"),
            BrightRed => Some("\u{1b}[101m"),
            BrightGreen => Some("\u{1b}[102m"),
            BrightYellow => Some("\u{1b}[103m"),
            BrightBlue => Some("\u{1b}[104m"),
            BrightMagenta => Some("\u{1b}[105m"),
            BrightCyan => Some("\u{1b}[106m"),
            BrightWhite => Some("\u{1b}[107m"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TermColor {
    fg: Color,
    bg: Color,
}

impl TermColor {
    pub fn reset() -> TermColor {
        TermColor {
            fg: Color::Reset,
            bg: Color::Reset,
        }
    }

    pub fn fg(color: Color) -> TermColor {
        TermColor {
            fg: color,
            bg: Color::None,
        }
    }

    pub fn bg(color: Color) -> TermColor {
        TermColor {
            fg: Color::None,
            bg: color,
        }
    }

    pub fn new(fg: Color, bg: Color) -> TermColor {
        TermColor { fg, bg }
    }
}

impl Display for TermColor {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match (self.fg.fg(), self.bg.bg()) {
            (Some(fg), Some(bg)) => write!(f, "{fg}{bg}"),
            (None, _) | (_, None) => write!(f, "\u{1b}[m"),
        }
    }
}

impl From<Color> for TermColor {
    fn from(color: Color) -> TermColor {
        TermColor {
            fg: color,
            bg: Color::None,
        }
    }
}

impl From<(Color, Color)> for TermColor {
    fn from(color: (Color, Color)) -> TermColor {
        let (fg, bg) = color;
        TermColor { fg, bg }
    }
}

#[derive(Debug)]
pub struct ColorString {
    sequence: Vec<(String, TermColor)>,
    color: bool,
}

impl Default for ColorString {
    fn default() -> Self {
        ColorString {
            sequence: Vec::with_capacity(2),
            color: std::env::var("NO_COLOR")
                .ok()
                .unwrap_or(String::from(""))
                .is_empty(),
        }
    }
}

impl ColorString {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_fg(string: impl ToString, fg: impl Into<TermColor>) -> Self {
        ColorString {
            sequence: vec![(string.to_string(), fg.into())],
            ..Default::default()
        }
    }

    pub fn push_string(&mut self, string: impl ToString, color: impl Into<TermColor>) {
        self.sequence.push((string.to_string(), color.into()))
    }

    pub fn push(&mut self, color_string: ColorString) {
        self.sequence.extend(color_string.sequence.into_iter())
    }
}

impl Display for ColorString {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if self.color {
            for (string, color) in self.sequence.iter() {
                write!(f, "{color}{string}")?;
            }
            write!(f, "{}", TermColor::reset())
        } else {
            for (string, _) in self.sequence.iter() {
                write!(f, "{string}")?;
            }
            Ok(())
        }
    }
}
