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

    /// An error encountered when converting a Path/Buf to an UTF-8 string.
    InvalidUtf8(Utf8Error),

    /// An error encountered when some entity is not found.
    NotFound(String),

    /// An error encountered when there is no LDraw catalog to resolve filenames from.
    NoLDrawCatalog,

    /// Unknown destination format for the convert command.
    UnknownConvertFormat,
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
            }) => {
                // TODO - Better formatting of this
                write!(f, "resolve error for filename '{}'", filename)?;
                if let Some(e) = resolve_error {
                    write!(f, "\n   > {}", e)?;
                    while let Some(e) = e.source() {
                        write!(f, "\n   > {}", e)?;
                    }
                }
                Ok(())
            }
            Error::JsonWrite(json_err) => write!(f, "error writing JSON: {}", json_err),
            Error::GltfWrite(io_err) => write!(f, "error writing glTF: {}", io_err),
            Error::InvalidUtf8(utf8_err) => write!(f, "invalid UTF-8 string: {}", utf8_err.context),
            Error::NotFound(desc) => write!(f, "{} (not found)", desc),
            Error::NoLDrawCatalog => write!(
                f,
                "No include/catalog path specified, and cannot use current directory. \
                Use -C/--catalog-path to specify the location of the catalog."
            ),
            Error::UnknownConvertFormat => {
                write!(
                    f,
                    "Only the value 'gltf' (glTF 2.0) is currently supported."
                )
            }
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

/// Error related to an invalid UTF-8 string.
///
/// This error is raised either when an argument passed to a function contains an invalid
/// UTF-8 sequence, or when such invalid sequence is obtained after manipulating other data.
#[derive(Debug)]
pub struct Utf8Error {
    /// Some free-form context about the operation which led to the invalid UTF-8 sequence.
    pub context: String,
}

impl Utf8Error {
    pub fn new(context: &str) -> Self {
        Self {
            context: context.to_string(),
        }
    }
}

impl From<Utf8Error> for Error {
    fn from(e: Utf8Error) -> Self {
        Error::InvalidUtf8(e)
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
            "test_file.ldr".to_string(),
            underlying,
        )))
    }

    #[test]
    fn test_error() {
        if let Err(e) = get_error() {
            eprintln!("Error: {}", e);
        };
    }

    #[test]
    fn test_source() {
        let resolve_error = ResolveError::new_raw("file");
        let error: Error = resolve_error.into();
        assert!(std::error::Error::source(&error).is_none());
    }

    #[derive(Debug, serde::Deserialize)]
    struct Dummy {}

    #[test]
    fn test_from() {
        let resolve_error = ResolveError::new_raw("file");
        let error: Error = resolve_error.into();
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::Resolve(_)));
        if let Error::Resolve(resolve_error) = &error {
            assert_eq!(resolve_error.filename, "file");
        }

        let parse_error = ParseError::new("file", error);
        let error: Error = parse_error.into();
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::Parse(_)));
        if let Error::Parse(parse_error) = &error {
            assert_eq!(parse_error.filename, "file");
        }

        let json_error = serde_json::from_str::<Dummy>("{[}").unwrap_err();
        let error: Error = json_error.into();
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::JsonWrite(_)));
        if let Error::JsonWrite(json_error) = &error {
            assert!(json_error.is_syntax());
        }

        let io_err = std::fs::File::open("_()__doesn't exist__()_").unwrap_err();
        let gltf_error = Error::GltfWrite(io_err);
        let error: Error = gltf_error;
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::GltfWrite(_)));
        if let Error::GltfWrite(gltf_error) = &error {
            assert_eq!(std::io::ErrorKind::NotFound, gltf_error.kind());
        }

        let utf8_err = Utf8Error::new("context string");
        let error: Error = utf8_err.into();
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::InvalidUtf8(_)));
        if let Error::InvalidUtf8(utf8_err) = &error {
            assert_eq!("context string", utf8_err.context);
        }

        let notfound_err = Error::NotFound("not found description".to_string());
        let error: Error = notfound_err;
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::NotFound(_)));
        if let Error::NotFound(notfound_err) = &error {
            assert_eq!("not found description", notfound_err);
        }

        let nocat_err = Error::NoLDrawCatalog;
        let error: Error = nocat_err;
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::NoLDrawCatalog));

        let unknfmt_err = Error::UnknownConvertFormat;
        let error: Error = unknfmt_err;
        eprintln!("err: {}", error);
        assert!(matches!(&error, Error::UnknownConvertFormat));
    }
}
