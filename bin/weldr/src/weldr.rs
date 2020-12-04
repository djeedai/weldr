//! The weldr tool for LDraw format management.
//!

#![allow(dead_code)]

mod error;
mod gltf;

#[macro_use]
extern crate log;

use error::Error;

use ansi_term::Color::{Blue, Purple, Red, Yellow};
use log::{Level, Metadata, Record};
use ordered_float::NotNan;
use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    io::{BufReader, Write},
    path::{Path, PathBuf},
};
use structopt::StructOpt;
use weldr::{Command, DrawContext, FileRefResolver, Mat4, ResolveError, Vec3, Vec4};

struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let prefix = match record.level() {
                log::Level::Error => format!("{}: ", Red.paint("error")),
                log::Level::Warn => format!("{}: ", Yellow.paint("warning")),
                log::Level::Info => "".to_string(),
                log::Level::Debug => format!("{}: ", Blue.paint("debug")),
                log::Level::Trace => format!("{}: ", Purple.paint("trace")),
            };
            eprintln!("{}{}", prefix, record.args());
        }
    }

    fn flush(&self) {}
}

#[derive(StructOpt)]
#[structopt(name = "weldr", author = "Jerome Humbert <djeedai@gmail.com>")]
struct CliArgs {
    #[structopt(subcommand)]
    cmd: Cmd,
}

#[derive(StructOpt)]
enum Cmd {
    Convert(ConvertCommand),
}

#[derive(StructOpt)]
#[structopt(name = "convert", about = "Convert LDraw file to another format")]
struct ConvertCommand {
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

struct App<'a, 'b> {
    cli: clap::App<'a, 'b>,
}

impl App<'_, '_> {
    #[cfg(not(tarpaulin_include))] // don't test function that exit process
    fn print_help_and_exit(&mut self) {
        let _ = self.cli.print_help();
        std::process::exit(1);
    }

    #[cfg(not(tarpaulin_include))] // don't test function that exit process
    fn print_error_and_exit(&self, msg: &str) {
        error!("{}", msg);
        std::process::exit(1);
    }

    #[cfg(not(tarpaulin_include))] // don't test function that exit process
    fn exit(&self, code: i32) {
        std::process::exit(code);
    }
}

/// Disk-based file reference resolver.
struct DiskResolver {
    base_paths: Vec<PathBuf>,
}

impl DiskResolver {
    fn new() -> DiskResolver {
        DiskResolver { base_paths: vec![] }
    }

    fn new_from_root<P: AsRef<Path>>(root_path: P) -> DiskResolver {
        let root = root_path.as_ref().to_path_buf();
        let base_paths = vec![
            root.join("p"),
            root.join("p").join("48"),
            root.join("parts"),
            root.join("parts").join("s"),
        ];
        DiskResolver { base_paths }
    }

    fn add_path<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref();
        if self.base_paths.iter().find(|p| *p == path).is_none() {
            self.base_paths.push(path.to_path_buf());
        }
    }

    /// Resolve a relative LDraw filename reference into an actual path on disk.
    fn resolve_path(&self, filename: &str) -> Result<PathBuf, ResolveError> {
        for prefix in &self.base_paths {
            let full_path = prefix.join(filename);
            if full_path.as_path().is_file() {
                return Ok(full_path);
            }
        }
        Err(ResolveError::new_raw(filename))
    }
}

impl FileRefResolver for DiskResolver {
    fn resolve(&self, filename: &str) -> Result<Vec<u8>, ResolveError> {
        for prefix in &self.base_paths {
            let full_path = prefix.join(filename);
            let file = File::open(full_path);
            if file.is_err() {
                continue;
            }
            let file = file.unwrap();
            let mut buf_reader = BufReader::new(file);
            let mut buffer = Vec::new();
            match buf_reader.read_to_end(&mut buffer) {
                Ok(_) => return Ok(buffer),
                Err(e) => return Err(ResolveError::new(filename, e)),
            }
        }
        Err(ResolveError::new(
            filename,
            std::io::Error::from(std::io::ErrorKind::NotFound),
        ))
    }
}

/// Helper to hold a [`Vec3`] which can be hashed and totally equated.
#[derive(Hash, PartialEq, Eq)]
struct VecRef {
    x: NotNan<f32>,
    y: NotNan<f32>,
    z: NotNan<f32>,
}

impl std::convert::From<&VecRef> for Vec3 {
    fn from(vec: &VecRef) -> Vec3 {
        Vec3 {
            x: vec.x.into(),
            y: vec.y.into(),
            z: vec.z.into(),
        }
    }
}

impl std::convert::From<Vec3> for VecRef {
    fn from(vec: Vec3) -> VecRef {
        // TODO - handle NaN to avoid panic on unwrap()
        VecRef {
            x: NotNan::new(vec.x).unwrap(),
            y: NotNan::new(vec.y).unwrap(),
            z: NotNan::new(vec.z).unwrap(),
        }
    }
}

impl std::convert::From<&Vec3> for VecRef {
    fn from(vec: &Vec3) -> VecRef {
        // TODO - handle NaN to avoid panic on unwrap()
        VecRef {
            x: NotNan::new(vec.x).unwrap(),
            y: NotNan::new(vec.y).unwrap(),
            z: NotNan::new(vec.z).unwrap(),
        }
    }
}

struct GeometryBuffer {
    size: usize,
    offset: usize,
    stride: usize,
    component_type: gltf::ComponentType,
    attribute_type: gltf::AttributeType,
}

struct GeometryCache {
    vertices: Vec<Vec3>,
    vertex_map: HashMap<VecRef, u32>,
    line_indices: Vec<u32>,
    triangle_indices: Vec<u32>,
}

impl GeometryCache {
    fn new() -> GeometryCache {
        GeometryCache {
            vertices: vec![],
            vertex_map: HashMap::new(),
            line_indices: vec![],
            triangle_indices: vec![],
        }
    }

    /// Insert a new vertex and return its index.
    fn insert_vertex(&mut self, vec: &Vec3, transform: &Mat4) -> u32 {
        let vec4 = Vec4::new(vec.x, vec.y, vec.z, 1.0);
        let vec = (transform * vec4).truncate();
        match self.vertex_map.get(&vec.into()) {
            Some(index) => *index,
            None => {
                let index = self.vertices.len();
                self.vertices.push(vec.clone());
                let index = index as u32;
                self.vertex_map.insert(vec.into(), index);
                index
            }
        }
    }

    fn add_line(&mut self, draw_ctx: &DrawContext, vertices: &[Vec3; 2]) {
        let i0 = self.insert_vertex(&vertices[0], &draw_ctx.transform);
        let i1 = self.insert_vertex(&vertices[1], &draw_ctx.transform);
        self.line_indices.push(i0);
        self.line_indices.push(i1);
    }

    fn add_triangle(&mut self, draw_ctx: &DrawContext, vertices: &[Vec3; 3]) {
        let i0 = self.insert_vertex(&vertices[0], &draw_ctx.transform);
        let i1 = self.insert_vertex(&vertices[1], &draw_ctx.transform);
        let i2 = self.insert_vertex(&vertices[2], &draw_ctx.transform);
        self.triangle_indices.push(i0);
        self.triangle_indices.push(i1);
        self.triangle_indices.push(i2);
    }

    fn add_quad(&mut self, draw_ctx: &DrawContext, vertices: &[Vec3; 4]) {
        self.add_triangle(draw_ctx, &[vertices[0], vertices[1], vertices[2]]);
        self.add_triangle(draw_ctx, &[vertices[0], vertices[2], vertices[3]]);
    }

    fn write(&self, base_path: &Path) -> Result<(), Error> {
        let json_path = base_path.with_extension("gltf");

        let bin_file_path = base_path.with_extension("glbuf");
        info!("Writing binary buffer file to {:?}", bin_file_path);
        let mut bin_file = File::create(&bin_file_path).map_err(|e| Error::GltfWrite(e))?;
        let (vb_size, line_ib_size, tri_ib_size) = self.write_binary_buffers(&mut bin_file)?;

        let mut offset: usize = 0;
        let vertex_buffer = GeometryBuffer {
            size: vb_size,
            offset,
            stride: 12,
            component_type: gltf::ComponentType::Float,
            attribute_type: gltf::AttributeType::Vec3,
        };
        offset += vb_size;

        let mut index_buffers = vec![];
        if !self.line_indices.is_empty() {
            index_buffers.push(GeometryBuffer {
                size: line_ib_size,
                offset,
                stride: 4,
                component_type: gltf::ComponentType::UnsignedInt,
                attribute_type: gltf::AttributeType::Scalar,
            });
            offset += line_ib_size;
        }
        if !self.triangle_indices.is_empty() {
            index_buffers.push(GeometryBuffer {
                size: tri_ib_size,
                offset,
                stride: 4,
                component_type: gltf::ComponentType::UnsignedInt,
                attribute_type: gltf::AttributeType::Scalar,
            });
            //offset += tri_ib_size;
        }

        info!("Writing JSON file to {:?}", json_path);
        let mut json_file = File::create(json_path).unwrap();
        self.write_gltf(
            &mut json_file,
            &bin_file_path,
            &vertex_buffer,
            &index_buffers,
        )
    }

    fn write_gltf<W: Write>(
        &self,
        w: &mut W,
        bin_file_path: &PathBuf,
        vertex_buffer: &GeometryBuffer,
        index_buffers: &[GeometryBuffer],
    ) -> Result<(), Error> {
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
                (&self.line_indices, gltf::PrimitiveMode::Lines, 1),
                (
                    &self.triangle_indices,
                    gltf::PrimitiveMode::Triangles,
                    if self.line_indices.is_empty() { 1 } else { 2 },
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
            uri: Some(bin_file_path.to_str().unwrap().to_string()),
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
        let buf = &json[..];
        w.write_all(buf).map_err(|e| Error::GltfWrite(e))
    }

    fn write_binary_buffers<W: Write>(&self, w: &mut W) -> Result<(usize, usize, usize), Error> {
        let vertices = &self.vertices[..];
        let vertices_bytes: &[u8] = unsafe { as_u8_slice(vertices) };
        w.write_all(vertices_bytes)
            .map_err(|e| Error::GltfWrite(e))?;
        let line_indices = &self.line_indices[..];
        let line_indices_bytes: &[u8] = unsafe { as_u8_slice(line_indices) };
        w.write_all(line_indices_bytes)
            .map_err(|e| Error::GltfWrite(e))?;
        let triangle_indices = &self.triangle_indices[..];
        let triangle_indices_bytes: &[u8] = unsafe { as_u8_slice(triangle_indices) };
        w.write_all(triangle_indices_bytes)
            .map_err(|e| Error::GltfWrite(e))?;
        Ok((
            vertices_bytes.len(),
            line_indices_bytes.len(),
            triangle_indices_bytes.len(),
        ))
    }
}

/// Transform a slice of something sized into a slice of u8 for binary writing.
unsafe fn as_u8_slice<T: Sized>(p: &[T]) -> &[u8] {
    ::std::slice::from_raw_parts(
        p.as_ptr() as *const u8,
        ::std::mem::size_of::<T>() * p.len(),
    )
}

fn convert(app: &mut App, args: &ConvertCommand) -> Result<(), Error> {
    let input = args.input.to_str();
    if input.is_none() {
        app.print_error_and_exit("Input filename contains invalid UTF-8 characters.");
    }
    let input = input.unwrap();

    let mut resolver = DiskResolver::new();
    if let Some(catalog_path) = &args.catalog_path {
        resolver = DiskResolver::new_from_root(catalog_path);
    } else if let Ok(cwd) = std::env::current_dir() {
        warn!(
            "No catalog path specified; using current working directory: {}",
            cwd.to_str().unwrap_or("(invalid path)")
        );
        resolver = DiskResolver::new_from_root(cwd);
    } else {
        app.print_error_and_exit(
            "No include/catalog path specified, and cannot use current directory. \
        Use -C/--catalog-path to specify the location of the catalog.",
        )
    }
    if let Some(include_paths) = &args.include_paths {
        for path in include_paths {
            resolver.add_path(path);
        }
    }

    info!("Parsing file '{}'", input);
    let mut source_map = weldr::SourceMap::new();
    let source_file_ref = weldr::parse(input, &resolver, &mut source_map)?;

    info!("Converting file '{}' to {} format", input, "gltf");
    let mut geometry_cache = GeometryCache::new();
    let source_file = source_file_ref.get(&source_map);
    for (draw_ctx, cmd) in source_file.iter(&source_map) {
        debug!("  cmd: {:?}", cmd);
        match cmd {
            Command::Line(l) => {
                if args.with_lines {
                    geometry_cache.add_line(&draw_ctx, &l.vertices)
                }
            }
            Command::Triangle(t) => geometry_cache.add_triangle(&draw_ctx, &t.vertices),
            Command::Quad(q) => geometry_cache.add_quad(&draw_ctx, &q.vertices),
            Command::OptLine(l) => {
                if args.with_lines {
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

    let json_path = resolver.resolve_path(input)?;
    geometry_cache.write(&json_path)
}

static LOGGER: SimpleLogger = SimpleLogger;

#[cfg(not(tarpaulin_include))]
fn main() -> Result<(), Error> {
    log::set_logger(&LOGGER)
        .map(|()| log::set_max_level(log::LevelFilter::Info))
        .unwrap();

    let args = CliArgs::from_args();
    let mut app = App {
        cli: CliArgs::clap(),
    };

    let res = match args.cmd {
        Cmd::Convert(conv) => convert(&mut app, &conv),
    };
    if let Err(e) = res {
        match e {
            Error::Parse(parse_err) => {
                app.print_error_and_exit(
                    &format!("failed to parse LDraw file '{}'.", parse_err.filename)[..],
                );
            }
            Error::Resolve(resolve_err) => {
                error!("cannot resolve filename '{}'.", resolve_err.filename);
                if let Some(e) = resolve_err.resolve_error {
                    error!("       {}", e);
                    while let Some(e) = e.source() {
                        error!("       {}", e);
                    }
                }
                app.exit(1);
            }
            Error::JsonWrite(json_err) => {
                app.print_error_and_exit(&format!("failed to write JSON: {}", json_err)[..]);
            }
            Error::GltfWrite(io_err) => {
                app.print_error_and_exit(&format!("failed to write glTF: {}", io_err)[..]);
            }
        }
    }

    let buf = gltf::Buffer::new(32).uri("buf1.glb");
    let asset = gltf::Asset::new("2.0").generator("weldr");
    let mut gltf = gltf::Gltf::new(asset);
    gltf.buffers.push(buf);
    let json = serde_json::to_string_pretty(&gltf);
    println!("json={:#?}", json.unwrap());

    //println!("args: pattern={}, path={}", args.pattern, args.path.to_str().unwrap());
    let cmds = weldr::parse_raw(&b"1 16 0 0 0 1 0 0 0 1 0 sub.ldr"[..])?;
    println!("cmds #{}", cmds.len());
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use log::Log;

    #[test]
    fn test_as_u8_slice() {
        assert_eq!(12, std::mem::size_of::<Vec3>());
        let v = vec![Vec3::new(1.0, 2.0, 4.0), Vec3::new(1.0, 2.0, 4.0)];
        let b: &[u8] = unsafe { as_u8_slice(&v[..]) };
        assert_eq!(24, b.len());
    }

    #[test]
    fn test_disk_resolver_new() {
        assert!(DiskResolver::new().base_paths.is_empty())
    }

    #[test]
    fn test_disk_resolver_add_path() {
        let mut resolver = DiskResolver::new();
        assert!(resolver.base_paths.is_empty());
        resolver.add_path("path1");
        assert_eq!(1, resolver.base_paths.len());
        assert_eq!("path1", resolver.base_paths[0].to_str().unwrap());
        resolver.add_path("path1"); // duplicate is no-op
        assert_eq!(1, resolver.base_paths.len());
        assert_eq!("path1", resolver.base_paths[0].to_str().unwrap());
        resolver.add_path("path2");
        assert_eq!(2, resolver.base_paths.len());
        assert_eq!("path1", resolver.base_paths[0].to_str().unwrap());
        assert_eq!("path2", resolver.base_paths[1].to_str().unwrap());
    }

    #[test]
    fn test_disk_resolver_new_from_root() {
        let mut resolver = DiskResolver::new_from_root("root");
        assert_eq!(4, resolver.base_paths.len());
        assert!(resolver
            .base_paths
            .iter()
            .find(|&p| { *p == Path::new("root").join("p") })
            .is_some());
        assert!(resolver
            .base_paths
            .iter()
            .find(|&p| { *p == Path::new("root").join("p").join("48") })
            .is_some());
        assert!(resolver
            .base_paths
            .iter()
            .find(|&p| { *p == Path::new("root").join("parts") })
            .is_some());
        assert!(resolver
            .base_paths
            .iter()
            .find(|&p| { *p == Path::new("root").join("parts").join("s") })
            .is_some());
        resolver.add_path("path1");
        assert_eq!(5, resolver.base_paths.len());
        assert!(resolver
            .base_paths
            .iter()
            .find(|&p| { *p == Path::new("path1") })
            .is_some());
    }

    #[test]
    fn test_disk_resolver_resolve_path() {
        // Ensure test dir exists and is empty
        let mut root_path = std::env::temp_dir();
        root_path.push("weldr/tests/disk_resolver");
        if root_path.is_dir() {
            std::fs::remove_dir(&root_path).unwrap_or_default();
        }
        std::fs::create_dir_all(&root_path).unwrap_or_default();
        let base_path = root_path.join("parts");
        std::fs::create_dir(&base_path).unwrap_or_default();

        // Create disk-based resolver
        let resolver = DiskResolver::new_from_root(root_path.to_str().unwrap());

        // Create a dummy file and resolve its reference filename to the on-disk filename
        let dummy_filename = base_path.join("dummy.ldr");
        {
            let mut f = std::fs::File::create(&dummy_filename).unwrap();
            f.write(b"dummy content").unwrap();
        }
        assert_eq!(dummy_filename, resolver.resolve_path("dummy.ldr").unwrap());

        // Resolve its content
        assert!(resolver.resolve("dummy.ldr").is_ok());

        // Delete test file
        std::fs::remove_file(&dummy_filename).unwrap_or_default();

        // Fail to resolve non-existing file
        let dummy_filename = base_path.join("non_existing.ldr");
        if dummy_filename.is_file() {
            std::fs::remove_file(&dummy_filename).unwrap_or_default();
        }
        assert!(matches!(
            resolver.resolve("non_existing.ldr"),
            Err(weldr::ResolveError {
                filename: _,
                resolve_error: _,
            })
        ));

        // Delete the test dir on success
        std::fs::remove_dir(&base_path).unwrap_or_default();
    }

    // #[test]
    // fn test_convert() -> Result<(), Error> {
    //     let mut app = App {
    //         cli: CliArgs::clap(),
    //     };
    //     convert(&mut app, Path::new("6143.dat").to_path_buf())
    // }

    #[test]
    fn test_from_vecref() {
        let r = &VecRef {
            x: NotNan::new(0.0).unwrap(),
            y: NotNan::new(1.0).unwrap(),
            z: NotNan::new(2.0).unwrap(),
        };
        let v: Vec3 = r.into();
        assert_eq!(0.0, v.x);
        assert_eq!(1.0, v.y);
        assert_eq!(2.0, v.z);
    }

    #[test]
    fn test_from_vec3() {
        let v = Vec3 {
            x: 0.0,
            y: 1.0,
            z: 2.0,
        };
        let r: VecRef = v.into();
        assert_eq!(0.0, r.x.into_inner() as f32);
        assert_eq!(1.0, r.y.into_inner() as f32);
        assert_eq!(2.0, r.z.into_inner() as f32);
    }

    #[test]
    fn test_from_vec3_ref() {
        let v = &Vec3 {
            x: 0.0,
            y: 1.0,
            z: 2.0,
        };
        let r: VecRef = v.into();
        assert_eq!(0.0, r.x.into_inner() as f32);
        assert_eq!(1.0, r.y.into_inner() as f32);
        assert_eq!(2.0, r.z.into_inner() as f32);
    }

    #[test]
    fn test_geocache_insert() {
        let mut geo = GeometryCache::new();
        // First vertex always inserts
        let index = geo.insert_vertex(&Vec3::new(0.0, 1.0, 2.0), &Mat4::from_scale(1.0));
        assert_eq!(0, index);
        assert_eq!(1, geo.vertex_map.len());
        assert_eq!(1, geo.vertices.len());
        assert_eq!(0, geo.line_indices.len());
        assert_eq!(0, geo.triangle_indices.len());
        // Duplicate vertex
        let index = geo.insert_vertex(&Vec3::new(0.0, 1.0, 2.0), &Mat4::from_scale(1.0));
        assert_eq!(0, index);
        assert_eq!(1, geo.vertex_map.len());
        assert_eq!(1, geo.vertices.len());
        assert_eq!(0, geo.line_indices.len());
        assert_eq!(0, geo.triangle_indices.len());
        // New unique vertex
        let index = geo.insert_vertex(&Vec3::new(-5.0, 1.0, 2.0), &Mat4::from_scale(1.0));
        assert_eq!(1, index);
        assert_eq!(2, geo.vertices.len());
        assert_eq!(2, geo.vertex_map.len());
        assert_eq!(0, geo.line_indices.len());
        assert_eq!(0, geo.triangle_indices.len());
    }

    #[test]
    fn test_geocache_add_line() {
        let mut geo = GeometryCache::new();
        let draw_ctx = DrawContext {
            transform: Mat4::from_scale(1.0),
            color: 16,
        };
        geo.add_line(
            &draw_ctx,
            &[Vec3::new(0.0, 0.0, 0.0), Vec3::new(1.0, 0.0, 0.0)],
        );
        assert_eq!(2, geo.vertices.len());
        assert_eq!(2, geo.vertex_map.len());
        assert_eq!(2, geo.line_indices.len());
        assert_eq!(0, geo.triangle_indices.len());
    }

    #[test]
    fn test_geocache_add_triangle() {
        let mut geo = GeometryCache::new();
        let draw_ctx = DrawContext {
            transform: Mat4::from_scale(1.0),
            color: 16,
        };
        geo.add_triangle(
            &draw_ctx,
            &[
                Vec3::new(0.0, 0.0, 0.0),
                Vec3::new(1.0, 0.0, 0.0),
                Vec3::new(0.0, 1.0, 0.0),
            ],
        );
        assert_eq!(3, geo.vertices.len());
        assert_eq!(3, geo.vertex_map.len());
        assert_eq!(0, geo.line_indices.len());
        assert_eq!(3, geo.triangle_indices.len());
    }

    #[test]
    fn test_geocache_add_quad() {
        let mut geo = GeometryCache::new();
        let draw_ctx = DrawContext {
            transform: Mat4::from_scale(1.0),
            color: 16,
        };
        geo.add_quad(
            &draw_ctx,
            &[
                Vec3::new(0.0, 0.0, 0.0),
                Vec3::new(1.0, 0.0, 0.0),
                Vec3::new(1.0, 1.0, 0.0),
                Vec3::new(0.0, 1.0, 0.0),
            ],
        );
        // Quad is made of 2 triangles (6 indices)
        assert_eq!(4, geo.vertices.len());
        assert_eq!(4, geo.vertex_map.len());
        assert_eq!(0, geo.line_indices.len());
        assert_eq!(6, geo.triangle_indices.len());
    }

    struct TestWriter {}

    impl std::io::Write for TestWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn test_writer() {
        let mut writer = TestWriter {};
        assert_eq!(4, writer.write(b"test").unwrap());
        assert!(writer.flush().is_ok());
    }

    #[test]
    fn test_geocache_write_all() {
        let mut geo = GeometryCache::new();
        let draw_ctx = DrawContext {
            transform: Mat4::from_scale(1.0),
            color: 16,
        };
        geo.add_quad(
            &draw_ctx,
            &[
                Vec3::new(0.0, 0.0, 0.0),
                Vec3::new(1.0, 0.0, 0.0),
                Vec3::new(1.0, 1.0, 0.0),
                Vec3::new(0.0, 1.0, 0.0),
            ],
        );
        let mut writer = TestWriter {};
        assert!(geo.write_binary_buffers(&mut writer).is_ok());
        let vertex_buffer = GeometryBuffer {
            offset: 0,
            size: 12,
            stride: 12,
            component_type: gltf::ComponentType::Float,
            attribute_type: gltf::AttributeType::Vec3,
        };
        let mut index_buffers = vec![];
        index_buffers.push(GeometryBuffer {
            offset: 0,
            size: 12,
            stride: 4,
            component_type: gltf::ComponentType::UnsignedInt,
            attribute_type: gltf::AttributeType::Scalar,
        });
        assert!(geo
            .write_gltf(&mut writer, &PathBuf::new(), &vertex_buffer, &index_buffers)
            .is_ok());
    }

    #[test]
    fn test_simple_logger() {
        let logger = SimpleLogger {};

        let rec = log::RecordBuilder::new()
            .file(Some("filename"))
            .line(Some(32))
            .build();
        logger.log(&rec);
        logger.flush();

        let metadata = log::MetadataBuilder::new()
            .level(log::Level::Error)
            .target("target")
            .build();
        assert!(logger.enabled(&metadata));
    }
}
