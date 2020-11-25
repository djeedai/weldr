//! Error management

use std::convert::From;
use std::fmt;

/// Generic error for all weldr operations.
#[derive(Debug)]
pub enum Error {
    /// An error encountered while parsing some LDraw file content.
    Parse(ParseError),

    /// An error encountered while resolving a sub-file reference.
    Resolve(ResolveError),
}

#[derive(Debug)]
pub struct ParseError {
    pub filename: String,
    pub parse_error: Option<Box<dyn std::error::Error>>,
}

#[derive(Debug)]
pub struct ResolveError {
    pub filename: String,
    pub resolve_error: Option<Box<dyn std::error::Error>>,
}

impl ParseError {
    /// Create a [`ParseError`] that stems from an arbitrary error of an underlying parser.
    pub fn new(filename: &str, err: impl Into<Box<dyn std::error::Error>>) -> Self {
        ParseError {
            filename: filename.to_string(),
            parse_error: Some(err.into()),
        }
    }

    /// Create a [`ParseError`] that stems from a [`nom`] parsing error, capturing the [`nom::error::ErrorKind`]
    /// from the underlying parser which failed.
    pub fn new_from_nom(filename: &str, err: &nom::Err<(&[u8], nom::error::ErrorKind)>) -> Self {
        ParseError {
            filename: filename.to_string(),
            parse_error: match err {
                nom::Err::Incomplete(_) => None,
                nom::Err::Error((_, e)) => {
                    // Discard input slice due to lifetime constraint
                    let e2: nom::Err<_> = nom::Err::Error(*e);
                    Some(e2.into())
                }
                nom::Err::Failure((_, e)) => {
                    // Discard input slice due to lifetime constraint
                    let e2: nom::Err<_> = nom::Err::Failure(*e);
                    Some(e2.into())
                }
            },
        }
    }
}

impl ResolveError {
    /// Create a [`ResolveError`] that stems from an arbitrary error of an underlying resolution error.
    pub fn new(filename: &str, err: impl Into<Box<dyn std::error::Error>>) -> Self {
        ResolveError {
            filename: filename.to_string(),
            resolve_error: Some(err.into()),
        }
    }

    /// Create a [`ResolveError`] without any underlying error.
    pub fn new_raw(filename: &str) -> Self {
        ResolveError {
            filename: filename.to_string(),
            resolve_error: None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parse(ParseError {
                filename,
                parse_error,
            }) => write!(f, "parse error in file '{}': {:?}", filename, parse_error),
            Error::Resolve(ResolveError {
                filename,
                resolve_error,
            }) => write!(
                f,
                "resolve error for filename '{}': {:?}",
                filename, resolve_error
            ),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
        // match self {
        //     Error::Parse(ParseError {
        //         filename,
        //         parse_error,
        //     }) => parse_error,
        //     Error::Resolve(ResolveError {
        //         filename,
        //         resolve_error,
        //     }) => resolve_error,
        // }
    }
}

impl From<ResolveError> for Error {
    fn from(e: ResolveError) -> Self {
        Error::Resolve(e)
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parse(e)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn get_error() -> Result<u32, Error> {
        let underlying = Error::Parse(ParseError {
            filename: "low_level.ldr".to_string(),
            parse_error: None,
        });
        Err(Error::Resolve(ResolveError::new(
            "test_file.ldr",
            underlying,
        )))
    }

    #[test]
    fn test_error() {
        match get_error() {
            Err(e) => println!("Error: {}", e),
            _ => {}
        };
    }

    #[test]
    fn test_new_from_nom() {
        let nom_error: nom::Err<(&[u8], nom::error::ErrorKind)> =
            nom::Err::Error((&b""[..], nom::error::ErrorKind::Alpha));
        let parse_error = ParseError::new_from_nom("file", &nom_error);
        assert_eq!(parse_error.filename, "file");
        assert!(parse_error.parse_error.is_some());
    }

    #[test]
    fn test_source() {
        let resolve_error = ResolveError::new_raw("file");
        let error: Error = resolve_error.into();
        assert!(std::error::Error::source(&error).is_none());
    }

    #[test]
    fn test_from() {
        let resolve_error = ResolveError::new_raw("file");
        let error: Error = resolve_error.into();
        println!("err: {}", error);
        match &error {
            Error::Resolve(resolve_error) => assert_eq!(resolve_error.filename, "file"),
            _ => panic!("Unexpected error type."),
        }

        let parse_error = ParseError::new("file", error);
        let error: Error = parse_error.into();
        println!("err: {}", error);
        match &error {
            Error::Parse(parse_error) => assert_eq!(parse_error.filename, "file"),
            _ => panic!("Unexpected error type."),
        }
    }
}
