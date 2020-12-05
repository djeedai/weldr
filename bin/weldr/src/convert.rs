//! Action to convert an LDraw file to another format.

use crate::error::{Error, Utf8Error};
use crate::{Action, DiskResolver, GeometryCache};

use std::path::PathBuf;
use structopt::StructOpt;
use weldr::Command;

#[derive(StructOpt)]
#[structopt(name = "convert", about = "Convert LDraw file to another format")]
pub(crate) struct ConvertCommand {
    /// Target format, one of "gltf"
    #[structopt(subcommand)]
    format: ConvertFormat,

    /// Input LDraw file to convert
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Output file, stdout if not present
    #[structopt(parse(from_os_str), long = "output", short = "o", value_name = "FILE")]
    output: Option<PathBuf>,

    /// Output line segments in addition of surfaces (triangles/quads).
    #[structopt(long = "lines", short = "l", takes_value = false)]
    with_lines: bool,

    /// Add the given location to the list of search paths for resolving parts.
    ///
    /// The location is appended to the list of search paths, in addition of the canonical paths derived
    /// from the catalog location. This option can be used multiple times to add more search paths.
    #[structopt(
        long = "include-path",
        short = "I",
        value_name = "PATH",
        // multiple=true + number_of_values=1 => can appear multiple times, with one value per occurrence
        multiple = true,
        number_of_values = 1
    )]
    include_paths: Option<Vec<PathBuf>>,

    /// Set the root location of the official LDraw catalog for resolving parts.
    ///
    /// This option adds the following search paths to the list of paths to resolve sub-file references from:
    /// - <PATH>/p
    /// - <PATH>/p/48
    /// - <PATH>/parts
    /// - <PATH>/parts/s
    #[structopt(long = "catalog-path", short = "C", value_name = "PATH")]
    catalog_path: Option<PathBuf>,
}

#[derive(StructOpt)]
#[structopt(display_order = 1)]
enum ConvertFormat {
    #[structopt(about = "Convert to glTF 2.0")]
    Gltf,
}

impl Action for ConvertCommand {
    fn exec(&mut self) -> Result<(), Error> {
        if !self.input.is_file() {
            return Err(Error::NotFound("cannot open input file".to_string()));
        }
        let input_str = self.input.to_str();
        if input_str.is_none() {
            // That should probably never happen since is_file() succeeded, unless on
            // a system that supports invalid UTF-8 paths (sounds unlikely).
            return Err(Error::InvalidUtf8(Utf8Error::new("input filename")));
        }
        let input_str = input_str.unwrap();

        let mut resolver: DiskResolver;
        if let Some(catalog_path) = &self.catalog_path {
            resolver = DiskResolver::new_from_root(catalog_path);
        } else if let Ok(cwd) = std::env::current_dir() {
            warn!(
                "No catalog path specified; using current working directory: {}",
                cwd.to_str().unwrap_or("(invalid path)")
            );
            resolver = DiskResolver::new_from_root(cwd);
        } else {
            // This is quite difficult to hit (and so, to test), since getting current_dir()
            // to fail is unlikely (docs say "dir does not exist or wrong permission", but the
            // former is quite difficult to get into since the OS will generally prevent delete
            // if some process has the folder as its current directory).
            return Err(Error::NoLDrawCatalog);
        }
        if let Some(include_paths) = &self.include_paths {
            for path in include_paths {
                resolver.add_path(path);
            }
        }

        info!("Parsing file '{}'", input_str);
        let mut source_map = weldr::SourceMap::new();
        let source_file_ref = weldr::parse(input_str, &resolver, &mut source_map)?;

        info!("Converting file '{}' to {} format", input_str, "gltf");
        let mut geometry_cache = GeometryCache::new();
        let source_file = source_file_ref.get(&source_map);
        for (draw_ctx, cmd) in source_file.iter(&source_map) {
            debug!("  cmd: {:?}", cmd);
            match cmd {
                Command::Line(l) => {
                    if self.with_lines {
                        geometry_cache.add_line(&draw_ctx, &l.vertices)
                    }
                }
                Command::Triangle(t) => geometry_cache.add_triangle(&draw_ctx, &t.vertices),
                Command::Quad(q) => geometry_cache.add_quad(&draw_ctx, &q.vertices),
                Command::OptLine(l) => {
                    if self.with_lines {
                        geometry_cache.add_line(&draw_ctx, &l.vertices)
                    }
                }
                _ => {}
            }
        }
        // for v in &geometry_cache.vertices {
        //     trace!("vec: {:?}", v);
        // }
        // for i in &geometry_cache.indices {
        //     trace!("idx: {:?}", i);
        // }

        geometry_cache.write(&self.input)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::testutils;
    use std::io::Write;

    #[test]
    fn test_input_file_not_found() {
        let mut cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: PathBuf::new(),
            output: None,
            with_lines: false,
            include_paths: None,
            catalog_path: None,
        };
        assert!(matches!(cmd.exec(), Err(Error::NotFound(_))));
    }

    #[test]
    fn test_invalid_input_content() {
        let test_path = testutils::setup_test_folder("convert");

        // Create dummy input file
        let dummy_filename = test_path.join("dummy.ldr");
        {
            let mut f = std::fs::File::create(&dummy_filename).unwrap();
            f.write(b"dummy content").unwrap();
        }

        // Convert
        let mut cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: dummy_filename.clone(),
            output: None,
            with_lines: false,
            include_paths: None,
            catalog_path: None,
        };
        // NOTE: Currently succeeds because parsing silently fails and return
        //       an empty file, instead of an error.
        assert!(matches!(cmd.exec(), Ok(_)));

        // Delete test file
        std::fs::remove_file(&dummy_filename).unwrap_or_default();
    }

    #[test]
    fn test_valid() {
        let test_path = testutils::setup_test_folder("convert");

        // Create input file
        let filename = test_path.join("valid.ldr");
        {
            let mut f = std::fs::File::create(&filename).unwrap();
            f.write(b"0 this is a comment").unwrap();
        }

        // Convert
        let mut cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: filename.clone(),
            output: None,
            with_lines: false,
            include_paths: None,
            catalog_path: Some(test_path.clone()),
        };
        assert!(matches!(cmd.exec(), Ok(_)));

        // Delete test file
        std::fs::remove_file(&filename).unwrap_or_default();
    }
}
