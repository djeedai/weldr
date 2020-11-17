//! Error management

use std::convert::From;
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct ResolveError {
    pub filename: String,
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "resolve error for filename {}", self.filename)
    }
}

impl Error for ResolveError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "==========ParseError===========")
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl From<nom::Err<(&[u8], nom::error::ErrorKind)>> for ParseError {
    fn from(_item: nom::Err<(&[u8], nom::error::ErrorKind)>) -> Self {
        ParseError {}
    }
}

impl From<ResolveError> for ParseError {
    fn from(_item: ResolveError) -> Self {
        ParseError {}
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn get_error() -> Result<u32, ParseError> {
        Err(ParseError {})
    }

    #[test]
    fn test_error() {
        match get_error() {
            Err(e) => println!("Error: {}", e),
            _ => {}
        };
    }
}
