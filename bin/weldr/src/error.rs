//! Error management

use std::convert::From;
use std::fmt;
use weldr::{ParseError, ResolveError};

/// Generic error for all weldr operations.
#[derive(Debug)]
pub enum Error {
    /// An error encountered while parsing some LDraw file content.
    Parse(ParseError),

    /// An error encountered while resolving a sub-file reference.
    Resolve(ResolveError),

    /// An error encountered in serde while exporting to JSON.
    JsonWrite(serde_json::Error),

    /// An error encountered while exporting to glTF.
    GltfWrite(std::io::Error),
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
            Error::JsonWrite(json_err) => write!(f, "error writing JSON: {}", json_err),
            Error::GltfWrite(io_err) => write!(f, "error writing glTF: {}", io_err),
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

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::JsonWrite(e)
    }
}

impl From<weldr::Error> for Error {
    fn from(e: weldr::Error) -> Self {
        match e {
            weldr::Error::Resolve(e) => Error::Resolve(e),
            weldr::Error::Parse(e) => Error::Parse(e),
        }
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
