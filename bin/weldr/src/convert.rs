//! Action to convert an LDraw file to another format.

use crate::{
    as_u8_slice,
    error::{Error, Utf8Error},
    gltf, Action, DiskResolver, GeometryBuffer, GeometryCache,
};

use std::{collections::HashMap, fs::File, io::Write, path::PathBuf};
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

#[derive(Debug, StructOpt)]
#[structopt(display_order = 1)]
enum ConvertFormat {
    #[structopt(about = "Convert to glTF 2.0")]
    Gltf,
}

impl ConvertCommand {
    fn write_impl(&self, blobs: &[&[u8]], file_ext: &str) -> Result<(), Error> {
        if let Some(output) = &self.output {
            let path = output.with_extension(file_ext);
            let mut file = File::create(path).unwrap();
            for data in blobs {
                file.write_all(data).map_err(|e| Error::GltfWrite(e))?;
            }
        } else {
            let mut stdout = std::io::stdout();
            for data in blobs {
                stdout.write_all(data).map_err(|e| Error::GltfWrite(e))?;
            }
        }
        Ok(())
    }

    fn get_bin_file_uri(&self) -> String {
        if let Some(output) = &self.output {
            output
                .with_extension("glbuf")
                .file_name()
                .unwrap_or(std::ffi::OsStr::new("buffer.glbuf"))
                .to_str()
                .unwrap_or("buffer.glbuf")
                .to_string()
        } else {
            "buffer.glbuf".to_string()
        }
    }

    fn write_gltf(&mut self, geometry_cache: &GeometryCache) -> Result<(), Error> {
        // Write binary buffers
        let vertices = &geometry_cache.vertices[..];
        let vertices_bytes: &[u8] = unsafe { as_u8_slice(vertices) };
        let line_indices = &geometry_cache.line_indices[..];
        let line_indices_bytes: &[u8] = unsafe { as_u8_slice(line_indices) };
        let triangle_indices = &geometry_cache.triangle_indices[..];
        let triangle_indices_bytes: &[u8] = unsafe { as_u8_slice(triangle_indices) };
        self.write_impl(
            &[vertices_bytes, line_indices_bytes, triangle_indices_bytes],
            "glbuf",
        )?;

        // Prepare geometry buffers for glTF writing
        let mut offset: usize = 0;
        let vertex_buffer = GeometryBuffer {
            size: vertices_bytes.len(),
            offset,
            stride: 12,
            component_type: gltf::ComponentType::Float,
            attribute_type: gltf::AttributeType::Vec3,
        };
        offset += vertex_buffer.size;

        let mut index_buffers = vec![];
        if !geometry_cache.line_indices.is_empty() {
            let buffer = GeometryBuffer {
                size: line_indices_bytes.len(),
                offset,
                stride: 4,
                component_type: gltf::ComponentType::UnsignedInt,
                attribute_type: gltf::AttributeType::Scalar,
            };
            offset += buffer.size;
            index_buffers.push(buffer);
        }
        if !geometry_cache.triangle_indices.is_empty() {
            let buffer = GeometryBuffer {
                size: triangle_indices_bytes.len(),
                offset,
                stride: 4,
                component_type: gltf::ComponentType::UnsignedInt,
                attribute_type: gltf::AttributeType::Scalar,
            };
            //offset += buffer.size;
            index_buffers.push(buffer);
        }

        let asset = gltf::Asset {
            version: "2.0".to_string(),
            min_version: None,
            generator: Some("weldr".to_string()),
            copyright: None,
        };
        let node = gltf::Node {
            name: None,
            children: vec![],
            mesh_index: Some(0),
        };
        let scene = gltf::Scene {
            name: None,
            nodes: vec![0],
        };
        let mut attributes: HashMap<String, u32> = HashMap::new();
        attributes.insert("POSITION".to_string(), 0);
        let mesh = gltf::Mesh {
            name: None,
            primitives: [
                (&geometry_cache.line_indices, gltf::PrimitiveMode::Lines, 1),
                (
                    &geometry_cache.triangle_indices,
                    gltf::PrimitiveMode::Triangles,
                    if geometry_cache.line_indices.is_empty() {
                        1
                    } else {
                        2
                    },
                ),
            ]
            .iter()
            .filter(|(buf, _, _)| !buf.is_empty())
            .map(|(_, mode, accessor_index)| gltf::Primitive {
                attributes: attributes.clone(),
                indices: *accessor_index,
                mode: *mode,
            })
            .collect(),
        };
        let total_index_byte_size: usize = index_buffers.iter().map(|buf| buf.size).sum();
        let total_byte_size = vertex_buffer.size + total_index_byte_size;
        let buffers = vec![gltf::Buffer {
            name: None,
            byte_length: total_byte_size as u32,
            uri: Some(self.get_bin_file_uri()),
        }];
        let buffer_views = vec![
            gltf::BufferView {
                name: Some("vertex_buffer".to_string()),
                //target: 34962, // ARRAY_BUFFER
                buffer_index: 0,
                byte_length: vertex_buffer.size as u32,
                byte_offset: 0,
                byte_stride: Some(vertex_buffer.stride as u32),
            },
            gltf::BufferView {
                name: Some("index_buffer".to_string()),
                //target: 34963, // ELEMENT_ARRAY_BUFFER
                buffer_index: 0,
                byte_length: total_index_byte_size as u32,
                byte_offset: vertex_buffer.size as u32,
                byte_stride: Some(4), // TODO: do not hardcode
            },
        ];
        let mut accessors = vec![gltf::Accessor {
            name: Some("vertex_data".to_string()),
            component_type: vertex_buffer.component_type,
            count: (vertex_buffer.size / vertex_buffer.stride) as u32,
            attribute_type: vertex_buffer.attribute_type,
            buffer_view_index: 0,
            byte_offset: 0,
            normalized: false,
        }];
        let mut byte_offset = 0;
        for buf in index_buffers {
            accessors.push(gltf::Accessor {
                name: Some("index_data".to_string()),
                component_type: buf.component_type,
                count: (buf.size / buf.stride) as u32,
                attribute_type: buf.attribute_type,
                buffer_view_index: 1,
                byte_offset,
                normalized: false,
            });
            byte_offset += buf.size as u32;
        }
        let gltf = gltf::Gltf {
            asset,
            nodes: vec![node],
            scenes: vec![scene],
            buffers,
            buffer_views,
            accessors,
            meshes: vec![mesh],
            scene: Some(0),
        };
        let json = serde_json::to_string_pretty(&gltf)?;
        let json = json.as_bytes();
        self.write_impl(&[json], "gltf")
    }
}

impl Action for ConvertCommand {
    fn exec(&mut self) -> Result<(), Error> {
        // Stringify input filename
        let input_str = self.input.to_str();
        if input_str.is_none() {
            // Note: this can't really be tested reliably, as on some OSes (e.g. Windows 10)
            // PathBuf.to_str() does *not* entail UTF-8 validation and returns Some(_) instead.
            return Err(Error::InvalidUtf8(Utf8Error::new("input filename")));
        }
        let input_str = input_str.unwrap();
        if input_str.is_empty() {
            return Err(Error::NotFound("empty input filename".to_string()));
        }

        // Setup disk resolver
        let mut resolver: DiskResolver;
        if let Some(catalog_path) = &self.catalog_path {
            resolver = DiskResolver::new_from_catalog(catalog_path)?;
        } else if let Ok(cwd) = std::env::current_dir() {
            warn!(
                "No catalog path specified; using current working directory: {}",
                cwd.to_str().unwrap_or("(invalid path)")
            );
            resolver = DiskResolver::new_from_catalog(cwd)?;
        } else {
            // This is quite difficult to hit (and so, to test), since getting current_dir()
            // to fail is unlikely (docs say "dir does not exist or wrong permission", but the
            // former is quite difficult to get into since the OS will generally prevent delete
            // if some process has the folder as its current directory).
            return Err(Error::NoLDrawCatalog);
        }
        if let Some(include_paths) = &self.include_paths {
            for path in include_paths {
                resolver.add_path(path).map_err(|e| {
                    Error::NotFound(format!("invalid include path '{:?}' ({})", path, e))
                })?;
            }
        }

        // Parse recursively
        info!("Parsing file '{}'", input_str);
        let mut source_map = weldr::SourceMap::new();
        let source_file_ref = weldr::parse(input_str, &resolver, &mut source_map)?;

        // Populate the geometry cache with the parsed data
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

        info!(
            "Converting file '{}' to {:?} format",
            input_str, self.format
        );
        match self.format {
            ConvertFormat::Gltf => self.write_gltf(&geometry_cache),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::testutils;
    use std::io::Write;
    use weldr::{DrawContext, Mat4, Vec3};

    #[test]
    fn test_input_file_empty() {
        let mut cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: PathBuf::new(),
            output: None,
            with_lines: false,
            include_paths: None,
            catalog_path: None,
        };
        let res = cmd.exec();
        assert!(matches!(res, Err(Error::NotFound(_))));
        if let Error::NotFound(desc) = res.unwrap_err() {
            assert!(desc.contains("empty"));
        }
    }

    #[test]
    fn test_input_file_not_found() {
        let non_existing_filename = "__doesn't exist__";
        let mut cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: PathBuf::from(&non_existing_filename),
            output: None,
            with_lines: false,
            include_paths: None,
            catalog_path: None,
        };
        let res = cmd.exec();
        eprintln!("res={:?}", res);
        assert!(matches!(res, Err(Error::Resolve(_))));
        if let Error::Resolve(resolve_error) = res.unwrap_err() {
            assert_eq!(non_existing_filename, resolve_error.filename);
        }
    }

    #[test]
    fn test_invalid_input_content() {
        let test_folder = testutils::setup_test_folder("convert");

        // Create dummy input file
        let dummy_filename = test_folder.path().join("dummy.ldr");
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
        let test_folder = testutils::setup_test_folder("convert");

        // Create main file
        let mainfile = test_folder.path().join("main.ldr");
        {
            let mut f = std::fs::File::create(&mainfile).unwrap();
            f.write(b"0 this is a comment\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 subfile.ldr")
                .unwrap();
        }

        // Create special include path 'extra'
        let extra_path = test_folder.path().join("extra");
        std::fs::create_dir(&extra_path).unwrap();

        // Create sub-file 'extra/subfile.ldr'
        let subfile = extra_path.join("subfile.ldr");
        {
            let mut f = std::fs::File::create(&subfile).unwrap();
            f.write(b"0 this is a comment").unwrap();
        }

        // Convert
        let mut cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: mainfile.clone(),
            output: None,
            with_lines: false,
            include_paths: Some(vec![extra_path]),
            catalog_path: Some(test_folder.path()),
        };
        assert!(matches!(cmd.exec(), Ok(_)));

        // Delete test files
        std::fs::remove_file(&mainfile).unwrap_or_default();
        std::fs::remove_file(&subfile).unwrap_or_default();
    }

    // struct TestWriter {}

    // impl std::io::Write for TestWriter {
    //     fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
    //         Ok(buf.len())
    //     }

    //     fn flush(&mut self) -> std::io::Result<()> {
    //         Ok(())
    //     }
    // }

    // #[test]
    // fn test_writer() {
    //     let mut writer = TestWriter {};
    //     assert_eq!(4, writer.write(b"test").unwrap());
    //     assert!(writer.flush().is_ok());
    // }

    // #[test]
    // fn test_convert_write_all() {
    //     let mut geo = GeometryCache::new();
    //     let draw_ctx = DrawContext {
    //         transform: Mat4::from_scale(1.0),
    //         color: 16,
    //     };
    //     geo.add_quad(
    //         &draw_ctx,
    //         &[
    //             Vec3::new(0.0, 0.0, 0.0),
    //             Vec3::new(1.0, 0.0, 0.0),
    //             Vec3::new(1.0, 1.0, 0.0),
    //             Vec3::new(0.0, 1.0, 0.0),
    //         ],
    //     );
    //     let mut writer = TestWriter {};
    //     assert!(geo.write_all(&mut writer).is_ok());
    //     let vertex_buffer = GeometryBuffer {
    //         offset: 0,
    //         size: 12,
    //         stride: 12,
    //         component_type: gltf::ComponentType::Float,
    //         attribute_type: gltf::AttributeType::Vec3,
    //     };
    //     let mut index_buffers = vec![];
    //     index_buffers.push(GeometryBuffer {
    //         offset: 0,
    //         size: 12,
    //         stride: 4,
    //         component_type: gltf::ComponentType::UnsignedInt,
    //         attribute_type: gltf::AttributeType::Scalar,
    //     });
    //     assert!(geo
    //         .write_gltf(&mut writer, &PathBuf::new(), &vertex_buffer, &index_buffers)
    //         .is_ok());
    // }
}
