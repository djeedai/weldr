//! The weldr tool for LDraw format management.
//!

#![allow(dead_code)]

mod error;
mod gltf;

use error::Error;

use ansi_term::Color::Red;
use ordered_float::NotNan;
use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    io::{self, BufReader, Write},
    path::{Path, PathBuf},
};
use structopt::StructOpt;
use weldr::{CommandType, FileRefResolver, ResolveError, Vec3};

#[derive(StructOpt)]
#[structopt(name = "weldr", author = "Jerome Humbert <djeedai@gmail.com>")]
struct CliArgs {
    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(StructOpt)]
enum Command {
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
    #[structopt(parse(from_os_str), long = "output", short = "o")]
    output: Option<PathBuf>,
}

#[derive(StructOpt)]
#[structopt(display_order = 1)]
enum ConvertFormat {
    #[structopt(about = "gltf")]
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
        eprintln!("{}: {}", Red.paint("error"), msg);
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

    fn new_from_root(root_path: &str) -> DiskResolver {
        let root = PathBuf::from(root_path);
        let base_paths = vec![
            root.join("p"),
            root.join("p").join("48"),
            root.join("parts"),
            root.join("parts").join("s"),
        ];
        DiskResolver { base_paths }
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
                Err(e) => match e.kind() {
                    io::ErrorKind::NotFound => continue,
                    _ => return Err(ResolveError::new(filename, e)),
                },
            }
        }
        Err(ResolveError::new_raw(filename))
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
    file: PathBuf,
    size: usize,
    stride: usize,
    component_type: gltf::ComponentType,
    attribute_type: gltf::AttributeType,
}

struct GeometryCache {
    vertices: Vec<Vec3>,
    indices: Vec<u32>,
    vertex_map: HashMap<VecRef, u32>,
}

impl GeometryCache {
    /// Insert a new vertex and return its index.
    fn insert(&mut self, vec: &Vec3) -> u32 {
        if let Some(index) = self.vertex_map.get(&vec.into()) {
            self.indices.push(*index);
            *index
        } else {
            let index = self.vertices.len();
            self.vertices.push(vec.clone());
            let index = index as u32;
            self.vertex_map.insert(vec.into(), index);
            self.indices.push(index);
            index
        }
    }

    fn add_line(&mut self, vertices: &[Vec3; 2]) {
        self.insert(&vertices[0]);
        self.insert(&vertices[1]);
    }

    fn add_triangle(&mut self, vertices: &[Vec3; 3]) {
        self.insert(&vertices[0]);
        self.insert(&vertices[1]);
        self.insert(&vertices[2]);
    }

    fn add_quad(&mut self, vertices: &[Vec3; 4]) {
        self.insert(&vertices[0]);
        self.insert(&vertices[1]);
        self.insert(&vertices[2]);
        self.insert(&vertices[0]);
        self.insert(&vertices[2]);
        self.insert(&vertices[3]);
    }

    fn write(&self, base_path: &Path) -> Result<(), Error> {
        let json_path = base_path.with_extension("gltf");

        let mut buffers = vec![];

        let vb_file_path = base_path.with_extension("vb.glbuf");
        eprintln!("Writing VB file to {:?}", vb_file_path);
        let mut vb_file = File::create(&vb_file_path).map_err(|e| Error::GltfWrite(e))?;
        let vb_size = self.write_vertex_buffer(&mut vb_file)?;
        buffers.push(GeometryBuffer {
            file: vb_file_path,
            size: vb_size,
            stride: 12,
            component_type: gltf::ComponentType::Float,
            attribute_type: gltf::AttributeType::Vec3,
        });

        let ib_file_path = base_path.with_extension("ib.glbuf");
        eprintln!("Writing IB file to {:?}", ib_file_path);
        let mut ib_file = File::create(&ib_file_path).map_err(|e| Error::GltfWrite(e))?;
        let ib_size = self.write_index_buffer(&mut ib_file)?;
        buffers.push(GeometryBuffer {
            file: ib_file_path,
            size: ib_size,
            stride: 4,
            component_type: gltf::ComponentType::UnsignedInt,
            attribute_type: gltf::AttributeType::Scalar,
        });

        eprintln!("Writing JSON file to {:?}", json_path);
        let mut json_file = File::create(json_path).unwrap();
        self.write_gltf(&mut json_file, &buffers)
    }

    fn write_gltf<W: Write>(
        &self,
        w: &mut W,
        geometry_buffers: &[GeometryBuffer],
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
            primitives: vec![gltf::Primitive {
                attributes,
                indices: 1,
                mode: gltf::PrimitiveMode::Triangles,
            }],
        };
        let mut buffers = vec![];
        let mut buffer_views = vec![];
        let mut accessors = vec![];
        let mut buffer_index = 0;
        for buf in geometry_buffers {
            buffers.push(gltf::Buffer {
                name: None,
                byte_length: buf.size as u32,
                uri: Some(buf.file.to_str().unwrap().to_string()),
            });
            buffer_views.push(gltf::BufferView {
                name: None,
                buffer_index,
                byte_length: buf.size as u32,
                byte_offset: 0,
                byte_stride: Some(buf.stride as u32),
            });
            accessors.push(gltf::Accessor {
                name: None,
                component_type: buf.component_type,
                count: (buf.size / buf.stride) as u32,
                attribute_type: buf.attribute_type,
                buffer_view_index: buffer_index,
                byte_offset: 0,
                normalized: false,
            });
            buffer_index += 1;
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

    fn write_vertex_buffer<W: Write>(&self, w: &mut W) -> Result<usize, Error> {
        let vertices = &self.vertices[..];
        let bytes: &[u8] = unsafe { as_u8_slice(vertices) };
        w.write_all(bytes).map_err(|e| Error::GltfWrite(e))?;
        Ok(bytes.len())
    }

    fn write_index_buffer<W: Write>(&self, w: &mut W) -> Result<usize, Error> {
        let indices = &self.indices[..];
        let bytes: &[u8] = unsafe { as_u8_slice(indices) };
        w.write_all(bytes).map_err(|e| Error::GltfWrite(e))?;
        Ok(bytes.len())
    }
}

/// Transform a slice of something sized into a slice of u8 for binary writing.
unsafe fn as_u8_slice<T: Sized>(p: &[T]) -> &[u8] {
    ::std::slice::from_raw_parts(
        p.as_ptr() as *const u8,
        ::std::mem::size_of::<T>() * p.len(),
    )
}

fn convert(app: &mut App, input: PathBuf) -> Result<(), Error> {
    let input = input.to_str();
    if input.is_none() {
        app.print_error_and_exit("Input filename contains invalid UTF-8 characters.");
    }
    let input = input.unwrap();

    eprintln!("Parsing file '{}'", input);
    let resolver = DiskResolver::new_from_root("F:\\dev\\weldr\\data");
    let mut source_map = weldr::SourceMap::new();
    let source_file_ref = weldr::parse(input, &resolver, &mut source_map)?;

    eprintln!("Converting file '{}' to {} format", input, "gltf");
    let mut geometry_cache = GeometryCache {
        vertices: vec![],
        indices: vec![],
        vertex_map: HashMap::new(),
    };
    let source_file = source_file_ref.get(&source_map);
    for cmd in source_file.iter(&source_map) {
        eprintln!("  cmd: {:?}", cmd);
        match cmd {
            CommandType::Line(l) => geometry_cache.add_line(&l.vertices),
            CommandType::Triangle(t) => geometry_cache.add_triangle(&t.vertices),
            CommandType::Quad(q) => geometry_cache.add_quad(&q.vertices),
            CommandType::OptLine(l) => geometry_cache.add_line(&l.vertices),
            _ => {}
        }
    }
    for v in &geometry_cache.vertices {
        eprintln!("vec: {:?}", v);
    }
    for i in &geometry_cache.indices {
        eprintln!("idx: {:?}", i);
    }

    let json_path = resolver.resolve_path(input)?;
    geometry_cache.write(&json_path)
}

#[cfg(not(tarpaulin_include))]
fn main() -> Result<(), Error> {
    let args = CliArgs::from_args();
    let mut app = App {
        cli: CliArgs::clap(),
    };

    let res = match args.cmd {
        Command::Convert(conv) => convert(&mut app, conv.input),
    };
    if let Err(e) = res {
        match e {
            Error::Parse(parse_err) => {
                app.print_error_and_exit(
                    &format!("failed to parse LDraw file '{}'.", parse_err.filename)[..],
                );
            }
            Error::Resolve(resolve_err) => {
                eprintln!(
                    "{}: cannot resolve filename '{}'.",
                    Red.paint("error"),
                    resolve_err.filename
                );
                if let Some(e) = resolve_err.resolve_error {
                    eprintln!("       {}", e);
                    while let Some(e) = e.source() {
                        eprintln!("       {}", e);
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
    fn test_disk_resolver_new_from_root() {
        let resolver = DiskResolver::new_from_root("root");
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
    }

    // #[test]
    // fn test_convert() -> Result<(), Error> {
    //     let mut app = App {
    //         cli: CliArgs::clap(),
    //     };
    //     convert(&mut app, Path::new("6143.dat").to_path_buf())
    // }
}
