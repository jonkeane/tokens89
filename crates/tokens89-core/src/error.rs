use std::{fmt, io};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Malformed(&'static str),
    MalformedDetail(String),
    UnsupportedCharacter(char),
    InvalidName(String),
    UnsupportedToken(u8),
    UnsupportedVariableType(u8),
    Usage(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "I/O error: {e}"),
            Self::Malformed(s) => write!(f, "malformed input: {s}"),
            Self::MalformedDetail(s) => write!(f, "malformed input: {s}"),
            Self::UnsupportedCharacter(c) => {
                write!(f, "unsupported character U+{:04X} ({c})", *c as u32)
            }
            Self::InvalidName(s) => write!(f, "invalid calculator name: {s}"),
            Self::UnsupportedToken(t) => write!(f, "unsupported token 0x{t:02x}"),
            Self::UnsupportedVariableType(t) => {
                write!(f, "unsupported TI variable type 0x{t:02x}")
            }
            Self::Usage(s) => f.write_str(s),
        }
    }
}

impl Error {
    /// Constructs an invalid-name error that explains the fixed-width TI name field.
    pub(crate) fn name_too_long(name: &str, length: usize, maximum: usize) -> Self {
        Self::InvalidName(format!(
            "{name} ({length} calculator characters; maximum is {maximum})"
        ))
    }
}

impl std::error::Error for Error {}
impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}
pub type Result<T> = std::result::Result<T, Error>;
