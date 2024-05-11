//! Action to convert an LDraw file to another format.

use crate::{
    error::{Error, Utf8Error},
    gltf, Action, App, DiskResolver, GeometryCache,
};

use ansi_term::Style;
use std::{collections::HashMap, fs::File, io::Write, path::PathBuf};
use structopt::StructOpt;
use weldr::Command;

#[derive(StructOpt, Debug)]
#[structopt(name = "convert", about = "Convert LDraw file to another format", settings = &[clap::AppSettings::UnifiedHelpMessage])]
pub(crate) struct ConvertCommand {
    /// Target format; currently only "gltf" is supported.
    #[structopt(value_name = "FORMAT")]
    format: ConvertFormat,

    /// Input LDraw file to convert.
    #[structopt(parse(from_os_str), value_name = "INPUT")]
    input: PathBuf,

    /// Output file, stdout if not present.
    #[structopt(parse(from_os_str), long = "output", short = "o", value_name = "FILE")]
    output: Option<PathBuf>,

    /// Output line segments in addition of surfaces (triangles/quads).
    #[structopt(long = "lines", short = "l", takes_value = false)]
    lines_enabled: bool,

    /// Add the given location to the list of search paths for resolving parts.
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
    /// This option adds the "<PATH>/p", "<PATH>/p/48", "<PATH>/parts", and "<PATH>/parts/s" search paths
    /// to the list of paths to resolve sub-file references from.
    #[structopt(long = "catalog-path", short = "C", value_name = "PATH")]
    catalog_path: Option<PathBuf>,
}

#[derive(Debug, PartialEq, Eq, StructOpt)]
#[structopt(display_order = 1)]
enum ConvertFormat {
    #[structopt(about = "Convert to glTF 2.0")]
    Gltf,
}

impl std::str::FromStr for ConvertFormat {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "gltf" => Ok(ConvertFormat::Gltf),
            _ => Err(Error::UnknownConvertFormat),
        }
    }
}

impl std::fmt::Display for ConvertFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ConvertFormat::Gltf => write!(f, "glTF 2.0"),
        }
    }
}

impl ConvertCommand {
    fn write_impl(&self, data: &[u8], file_ext: &str) -> Result<(), Error> {
        if let Some(output) = &self.output {
            let path = output.with_extension(file_ext);
            let mut file = File::create(path).unwrap();
            file.write_all(data).map_err(Error::GltfWrite)?;
        } else {
            let mut stdout = std::io::stdout();
            stdout.write_all(data).map_err(Error::GltfWrite)?;
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

    fn add_nodes(
        &self,
        filename: &str,
        source_file: &weldr::SourceFile,
        transform: Option<weldr::Mat4>,
        source_map: &weldr::SourceMap,
        gltf: &mut gltf::Gltf,
        buffer: &mut Vec<u8>,
        mesh_cache: &mut HashMap<String, Option<u32>>,
    ) -> u32 {
        let matrix = transform.map(|m| m.to_cols_array());

        let node_index = gltf.nodes.len();
        let node = gltf::Node {
            name: Some(filename.into()),
            children: Vec::new(),
            mesh_index: None,
            matrix,
        };
        gltf.nodes.push(node);

        // Create geometry if any for this node
        let opt_mesh_index = mesh_cache.entry(filename.into()).or_insert_with(|| {
            let mesh_index = gltf.meshes.len() as u32;
            let geometry = self.create_geometry(source_file, source_map);
            // Don't set empty meshes to avoid import errors.
            if !geometry.vertices.is_empty() && !geometry.triangle_indices.is_empty() {
                self.add_mesh(&geometry, gltf, buffer);
                Some(mesh_index)
            } else {
                None
            }
        });
        gltf.nodes[node_index].mesh_index = *opt_mesh_index;

        // Recursively parse sub-files
        for cmd in &source_file.cmds {
            if let Command::SubFileRef(sfr_cmd) = cmd {
                if let Some(subfile) = source_map.get(&sfr_cmd.file) {
                    // Don't apply node transforms to preserve the scene hierarchy.
                    // Applications should handle combining the transforms.
                    let transform = sfr_cmd.matrix();

                    let child_node_index = self.add_nodes(
                        &sfr_cmd.file,
                        subfile,
                        Some(transform),
                        source_map,
                        gltf,
                        buffer,
                        mesh_cache,
                    );
                    gltf.nodes[node_index].children.push(child_node_index);
                }
            }
        }

        node_index as u32
    }

    fn write_gltf(
        &self,
        source_file: &weldr::SourceFile,
        source_map: &weldr::SourceMap,
    ) -> Result<(), Error> {
        let asset = gltf::Asset {
            version: "2.0".to_string(),
            min_version: None,
            generator: Some("weldr".to_string()),
            copyright: None,
        };
        let scene = gltf::Scene {
            name: None,
            nodes: vec![0],
        };

        let mut gltf = gltf::Gltf {
            asset,
            nodes: Vec::new(),
            scenes: vec![scene],
            buffers: Vec::new(),
            buffer_views: Vec::new(),
            accessors: Vec::new(),
            meshes: Vec::new(),
            scene: Some(0),
        };

        let mut buffer = Vec::new();

        // Avoid creating the same mesh more than once.
        // This also saves memory for importers with instancing support.
        let mut filename_to_mesh_index = HashMap::new();

        // Recursively add a node for each file.
        self.add_nodes(
            "root",
            source_file,
            None,
            source_map,
            &mut gltf,
            &mut buffer,
            &mut filename_to_mesh_index,
        );

        gltf.buffers.push(gltf::Buffer {
            name: None,
            byte_length: buffer.len() as u32,
            uri: Some(self.get_bin_file_uri()),
        });

        self.write_impl(&buffer, "glbuf")?;

        let json = serde_json::to_string_pretty(&gltf)?;
        self.write_impl(json.as_bytes(), "gltf")
    }

    fn create_geometry(
        &self,
        source_file: &weldr::SourceFile,
        source_map: &weldr::SourceMap,
    ) -> GeometryCache {
        let mut geometry_cache = GeometryCache::new();
        for (draw_ctx, cmd) in source_file.iter(source_map) {
            trace!("  cmd: {:?}", cmd);
            match cmd {
                Command::Line(l) => {
                    if self.lines_enabled {
                        geometry_cache.add_line(&draw_ctx, &l.vertices)
                    }
                }
                Command::Triangle(t) => geometry_cache.add_triangle(&draw_ctx, &t.vertices),
                Command::Quad(q) => geometry_cache.add_quad(&draw_ctx, &q.vertices),
                Command::OptLine(l) => {
                    if self.lines_enabled {
                        geometry_cache.add_line(&draw_ctx, &l.vertices)
                    }
                }
                _ => {}
            }
        }
        geometry_cache
    }

    fn add_mesh(
        &self,
        geometry_cache: &GeometryCache,
        gltf: &mut gltf::Gltf,
        buffer: &mut Vec<u8>,
    ) {
        // TODO: glTF is LE only; should convert on BE platforms
        let vertices = &geometry_cache.vertices;
        let vertices_bytes: &[u8] = bytemuck::cast_slice(&vertices[..]);

        // TODO: Line indices?
        let vertex_buffer_view_index = gltf.buffer_views.len() as u32;
        gltf.buffer_views.push(gltf::BufferView {
            name: Some("vertex_buffer".to_string()),
            buffer_index: 0,
            byte_length: vertices_bytes.len() as u32,
            byte_offset: buffer.len() as u32,
            byte_stride: Some(12),
            target: Some(gltf::BufferTarget::ArrayBuffer as u32),
        });
        buffer.extend_from_slice(vertices_bytes);

        let vertex_accessor = gltf::Accessor {
            name: Some("vertex_data".to_string()),
            component_type: gltf::ComponentType::Float,
            count: vertices.len() as u32,
            attribute_type: gltf::AttributeType::Vec3,
            buffer_view_index: vertex_buffer_view_index,
            byte_offset: 0,
            normalized: false,
            min: vertices
                .iter()
                .copied()
                .reduce(|a, b| weldr::Vec3::new(a.x.min(b.x), a.y.min(b.y), a.z.min(b.z)))
                .map(|v| [v.x, v.y, v.z]),
            max: vertices
                .iter()
                .copied()
                .reduce(|a, b| weldr::Vec3::new(a.x.max(b.x), a.y.max(b.y), a.z.max(b.z)))
                .map(|v| [v.x, v.y, v.z]),
        };

        let mut primitives = Vec::new();
        // TODO: Line indices.
        if !geometry_cache.triangle_indices.is_empty() {
            let attributes = HashMap::from([("POSITION".to_string(), gltf.accessors.len() as u32)]);
            gltf.accessors.push(vertex_accessor);

            // TODO: glTF is LE only; should convert on BE platforms
            let triangle_indices_bytes: &[u8] =
                bytemuck::cast_slice(&geometry_cache.triangle_indices[..]);

            let byte_offset = buffer.len() as u32;
            let byte_length = triangle_indices_bytes.len() as u32;
            let index_buffer_view_index = gltf.buffer_views.len() as u32;

            gltf.buffer_views.push(gltf::BufferView {
                name: Some("index_buffer".to_string()),
                buffer_index: 0,
                byte_length,
                byte_offset,
                byte_stride: None,
                target: Some(gltf::BufferTarget::ElementArrayBuffer as u32),
            });
            buffer.extend_from_slice(triangle_indices_bytes);

            let index_accessor = gltf::Accessor {
                name: Some("index_data".to_string()),
                component_type: gltf::ComponentType::UnsignedInt,
                count: geometry_cache.triangle_indices.len() as u32,
                attribute_type: gltf::AttributeType::Scalar,
                buffer_view_index: index_buffer_view_index,
                byte_offset: 0,
                normalized: false,
                min: None,
                max: None,
            };

            let primitive = gltf::Primitive {
                attributes,
                indices: gltf.accessors.len() as u32,
                mode: gltf::PrimitiveMode::Triangles,
            };
            primitives.push(primitive);
            gltf.accessors.push(index_accessor);
        }

        // TODO: mesh name?
        let mesh = gltf::Mesh {
            name: None,
            primitives,
        };

        gltf.meshes.push(mesh);
    }
}

impl Action for ConvertCommand {
    fn exec(&self, app: &App) -> Result<(), Error> {
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
        let mut resolver = if let Some(catalog_path) = &self.catalog_path {
            DiskResolver::new_from_catalog(catalog_path)
        } else if let Ok(cwd) = std::env::current_dir() {
            info!(
                "No catalog path specified; using current working directory '{}'",
                cwd.to_str().unwrap_or("(invalid path)")
            );
            let arg_style = Style::new().bold();
            app.tip(&format!(
                "Use {}/{} to specify the location of a catalog to resolve files.",
                arg_style.paint("--catalog-path"),
                arg_style.paint("-C")
            ));
            DiskResolver::new_from_catalog(cwd)
        } else {
            // This is quite difficult to hit (and so, to test), since getting current_dir()
            // to fail is unlikely (docs say "dir does not exist or wrong permission", but the
            // former is quite difficult to get into since the OS will generally prevent delete
            // if some process has the folder as its current directory).
            Err(Error::NoLDrawCatalog)
        }?;

        if let Some(include_paths) = &self.include_paths {
            for path in include_paths {
                resolver.add_path(path).map_err(|e| {
                    Error::NotFound(format!("invalid include path '{:?}' ({})", path, e))
                })?;
            }
        }

        // Output command info
        let output_str = if let Some(output) = &self.output {
            output
                .to_str()
                .map(|s| format!(" -> '{}'", s))
                .unwrap_or_default()
        } else {
            " -> (stdout)".to_string()
        };
        info!(
            "Converting file '{}' to {} format{}",
            input_str, self.format, output_str
        );

        // Parse recursively
        let mut source_map = weldr::SourceMap::new();
        let main_model_name = weldr::parse(input_str, &resolver, &mut source_map)?;
        let root_file = source_map.get(&main_model_name).unwrap();

        match self.format {
            ConvertFormat::Gltf => self.write_gltf(root_file, &source_map),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{testutils, App, CliArgs, Cmd, LoggerConfig};
    use std::io::Write;
    use std::str::FromStr;
    //use weldr::{DrawContext, Mat4, Vec3};

    #[test]
    fn test_convfmt_display() {
        let s = format!("{}", ConvertFormat::Gltf);
        assert!(s.contains("glTF 2.0"));
    }

    #[test]
    fn test_convfmt_fromstr() {
        assert_eq!(
            ConvertFormat::Gltf,
            ConvertFormat::from_str("gltf").unwrap()
        );
        assert!(ConvertFormat::from_str("").is_err());
        assert!(ConvertFormat::from_str("GLTF").is_err());
        assert!(ConvertFormat::from_str("__random__").is_err());
    }

    fn get_test_app<'a, 'b>() -> App<'a, 'b> {
        let args = CliArgs {
            cmd: Cmd::Convert(ConvertCommand {
                format: ConvertFormat::Gltf,
                input: PathBuf::new(),
                output: None,
                lines_enabled: false,
                include_paths: None,
                catalog_path: None,
            }),
            log_config: LoggerConfig {
                debug: false,
                trace: false,
                no_color: true,
                no_emoji: true,
            },
        };
        App {
            cli: CliArgs::clap(),
            args,
        }
    }

    #[test]
    fn test_input_file_empty() {
        let app = get_test_app();
        let cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: PathBuf::new(),
            output: None,
            lines_enabled: false,
            include_paths: None,
            catalog_path: None,
        };
        let res = cmd.exec(&app);
        assert!(matches!(res, Err(Error::NotFound(_))));
        if let Error::NotFound(desc) = res.unwrap_err() {
            assert!(desc.contains("empty"));
        }
    }

    #[test]
    fn test_input_file_not_found() {
        let app = get_test_app();
        let non_existing_filename = "__doesn't exist__";
        let cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: PathBuf::from(&non_existing_filename),
            output: None,
            lines_enabled: false,
            include_paths: None,
            catalog_path: None,
        };
        let res = cmd.exec(&app);
        eprintln!("res={:?}", res);
        assert!(matches!(res, Err(Error::Resolve(_))));
        if let Error::Resolve(resolve_error) = res.unwrap_err() {
            assert_eq!(non_existing_filename, resolve_error.filename);
        }
    }

    #[test]
    fn test_invalid_input_content() {
        let test_folder = testutils::setup_test_folder("invalid_input_content");

        // Create dummy input file
        let dummy_filename = test_folder.path().join("dummy.ldr");
        {
            let mut f = std::fs::File::create(&dummy_filename).unwrap();
            f.write(b"dummy content").unwrap();
        }

        // Convert
        let app = get_test_app();
        let cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: dummy_filename.clone(),
            output: None,
            lines_enabled: false,
            include_paths: None,
            catalog_path: None,
        };
        // NOTE: Currently succeeds because parsing silently fails and return
        //       an empty file, instead of an error.
        assert!(matches!(cmd.exec(&app), Ok(_)));

        // Delete test file
        std::fs::remove_file(&dummy_filename).unwrap_or_default();
    }

    #[test]
    fn test_valid() {
        let test_folder = testutils::setup_test_folder("convert_valid");

        // Create main file
        let mainfile = test_folder.path().join("main.ldr");
        {
            let mut f = std::fs::File::create(&mainfile).unwrap();
            f.write_all(
                br#"0 this is a comment
2 16 0 0 0 1 0 0
3 16 0 0 0 1 0 0 0 1 0
4 16 0 0 0 1 0 0 1 1 0 0 1 0
5 16 0 0 0 1 0 0
1 16 0 0 0 1 0 0 0 1 0 0 0 1 subfile.ldr
"#,
            )
            .unwrap();
        }

        // Create special include path 'extra'
        let extra_path = test_folder.path().join("extra");
        std::fs::create_dir(&extra_path).unwrap();

        // Create sub-file 'extra/subfile.ldr'
        let subfile = extra_path.join("subfile.ldr");
        {
            let mut f = std::fs::File::create(subfile).unwrap();
            f.write_all(b"0 this is a comment").unwrap();
        }

        // Convert
        let app = get_test_app();
        let cmd = ConvertCommand {
            format: ConvertFormat::Gltf,
            input: mainfile,
            output: Some(test_folder.path().join("main_cvt.gltf")),
            lines_enabled: false,
            include_paths: Some(vec![extra_path]),
            catalog_path: Some(test_folder.path()),
        };
        assert!(matches!(cmd.exec(&app), Ok(_)));
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
