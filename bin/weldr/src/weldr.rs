//! The weldr command-line tool for LDraw format management.

mod convert;
mod error;
mod gltf;

#[cfg(test)]
mod testutils;

#[macro_use]
extern crate log;

extern crate atty;

use convert::ConvertCommand;
use error::Error;

use ansi_term::Color::{Blue, Purple, Red, Yellow};
use log::{Metadata, Record};
use ordered_float::NotNan;
use std::{
    collections::HashMap,
    fs::File,
    io::BufReader,
    io::Read,
    path::{Path, PathBuf},
};
use structopt::StructOpt;
use weldr::{DrawContext, FileRefResolver, Mat4, ResolveError, Vec3, Vec4};

struct SimpleLogger {
    log_level: log::LevelFilter,
    color_enabled: bool,
    emoji_enabled: bool,
}

static mut LOGGER: SimpleLogger = SimpleLogger::new();

impl SimpleLogger {
    const fn new() -> SimpleLogger {
        SimpleLogger {
            log_level: log::LevelFilter::Info,
            color_enabled: false,
            emoji_enabled: false,
        }
    }

    fn init(args: &CliArgs) {
        unsafe {
            LOGGER.log_level = if args.trace {
                log::LevelFilter::Trace
            } else if args.debug {
                log::LevelFilter::Debug
            } else {
                log::LevelFilter::Info
            };
            // Only enable colored/emoji output on interactive terminal
            if atty::is(atty::Stream::Stderr) {
                LOGGER.color_enabled = !args.no_color;
                LOGGER.emoji_enabled = !args.no_emoji;
            }
            let logger: &'static SimpleLogger = &LOGGER;
            log::set_logger(logger)
                .map(|()| log::set_max_level(logger.log_level))
                .unwrap();
        }
    }
}

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= self.log_level
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let prefix = if self.emoji_enabled {
                match record.level() {
                    log::Level::Error => "❌ ".to_string(),
                    log::Level::Warn => "⚠ ".to_string(),
                    log::Level::Info => "".to_string(),
                    log::Level::Debug => format!("{}: ", Blue.paint("debug")),
                    log::Level::Trace => format!("{}: ", Purple.paint("trace")),
                }
            } else if self.color_enabled {
                match record.level() {
                    log::Level::Error => format!("{}: ", Red.paint("error")),
                    log::Level::Warn => format!("{}: ", Yellow.paint("warning")),
                    log::Level::Info => "".to_string(),
                    log::Level::Debug => format!("{}: ", Blue.paint("debug")),
                    log::Level::Trace => format!("{}: ", Purple.paint("trace")),
                }
            } else {
                match record.level() {
                    log::Level::Error => "[error] ".to_string(),
                    log::Level::Warn => "[warn ] ".to_string(),
                    log::Level::Info => "[info ] ".to_string(),
                    log::Level::Debug => "[debug] ".to_string(),
                    log::Level::Trace => "[trace] ".to_string(),
                }
            };
            eprintln!("{}{}", prefix, record.args());
        }
    }

    fn flush(&self) {}
}

#[derive(StructOpt, Debug)]
#[structopt(name = "weldr", author = "Jerome Humbert <djeedai@gmail.com>", settings = &[clap::AppSettings::UnifiedHelpMessage])]
pub(crate) struct CliArgs {
    #[structopt(subcommand)]
    cmd: Cmd,

    /// Enable debug logging.
    #[structopt(
        short = "d",
        long = "debug",
        takes_value = false,
        conflicts_with("trace"),
        group = "output"
    )]
    debug: bool,

    /// Enable trace logging (very verbose).
    #[structopt(
        long = "trace",
        takes_value = false,
        conflicts_with("debug"),
        group = "output"
    )]
    trace: bool,

    /// Do not output colors (implies --no-emoji).
    #[structopt(long = "no-color", takes_value = false, group = "output")]
    no_color: bool,

    /// Do not output emojis and other UTF-8 characters.
    #[structopt(long = "no-emoji", takes_value = false, group = "output")]
    no_emoji: bool,
}

#[derive(StructOpt, Debug)]
enum Cmd {
    Convert(ConvertCommand),
}

/// A top-level action to be performed by the executable, like e.g. 'convert'.
trait Action {
    fn exec(&self, app: &App) -> Result<(), Error>;
}

struct App<'a, 'b> {
    cli: clap::App<'a, 'b>,
    args: CliArgs,
}

impl App<'_, '_> {
    #[cfg(not(tarpaulin_include))] // don't test function that exit process
    fn exit(&self, code: i32) {
        std::process::exit(code);
    }

    /// Print on stderr some tip for user, e.g. when some argument is missing.
    fn tip(&self, tip: &str) {
        let prefix = if self.args.no_emoji { "" } else { "💡 " };
        eprintln!("{}{}", prefix, tip);
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

    fn new_from_catalog<P: AsRef<Path>>(catalog_path: P) -> Result<DiskResolver, Error> {
        let catalog_path = std::fs::canonicalize(catalog_path)
            .map_err(|e| Error::NotFound(format!("catalog path not found ({})", e)))?;
        let base_paths = vec![
            catalog_path.join("p"),
            catalog_path.join("p").join("48"),
            catalog_path.join("parts"),
            catalog_path.join("parts").join("s"),
        ];
        Ok(DiskResolver { base_paths })
    }

    fn add_path<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Error> {
        let path = std::fs::canonicalize(path)
            .map_err(|e| Error::NotFound(format!("include path not found ({})", e)))?;
        if self.base_paths.iter().find(|p| *p == &path).is_none() {
            self.base_paths.push(path.to_path_buf());
        }
        Ok(())
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
}

/// Transform a slice of something sized into a slice of u8, for binary writing.
unsafe fn as_u8_slice<T: Sized>(p: &[T]) -> &[u8] {
    ::std::slice::from_raw_parts(
        p.as_ptr() as *const u8,
        ::std::mem::size_of::<T>() * p.len(),
    )
}

#[cfg(not(target_os = "windows"))]
fn is_tty() -> bool {
    atty::is(atty::Stream::Stderr)
}

#[cfg(target_os = "windows")]
fn is_tty() -> bool {
    // On Windows (10), also check that ansi_term can output ANSI characters,
    // otherwise it will output raw escape codes and it will look like garbage.
    atty::is(atty::Stream::Stderr) && ansi_term::enable_ansi_support().is_ok()
}

#[cfg(not(tarpaulin_include))]
fn main() -> Result<(), Error> {
    let mut args = CliArgs::from_args();
    // Disable color/emoji if terminal cannot support them
    if !is_tty() {
        args.no_color = true;
        args.no_emoji = true;
    }
    // TODO - do that via clap/structopt directly instead?
    if args.no_color {
        // no_color implies no_emoji
        args.no_emoji = true;
    }
    SimpleLogger::init(&args);

    let app = App {
        cli: CliArgs::clap(),
        args,
    };

    debug!("Running command: {:?}", &app.args.cmd);
    let res = match &app.args.cmd {
        Cmd::Convert(conv) => conv.exec(&app),
    };
    if let Err(e) = res {
        error!("{}", e);
        app.exit(1);
    }

    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use log::Log;
    use std::io::Write;

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
        let test_folder = testutils::setup_test_folder("disk_resolver_add_path");
        let path1 = test_folder.path().join("path1");
        let path2 = test_folder.path().join("path2");
        std::fs::create_dir(&path1).unwrap();
        std::fs::create_dir(&path2).unwrap();
        let mut resolver = DiskResolver::new();
        assert!(resolver.base_paths.is_empty());
        resolver.add_path(&path1).unwrap();
        assert_eq!(1, resolver.base_paths.len());
        assert!(resolver.base_paths[0].to_str().unwrap().ends_with("path1"));
        resolver.add_path(&path1).unwrap(); // duplicate is no-op
        assert_eq!(1, resolver.base_paths.len());
        assert!(resolver.base_paths[0].to_str().unwrap().ends_with("path1"));
        resolver.add_path(&path2).unwrap();
        assert_eq!(2, resolver.base_paths.len());
        assert!(resolver.base_paths[0].to_str().unwrap().ends_with("path1"));
        assert!(resolver.base_paths[1].to_str().unwrap().ends_with("path2"));
    }

    #[test]
    fn test_disk_resolver_new_from_catalog() {
        let test_folder = testutils::setup_test_folder("disk_resolver_new_from_catalog");
        let paths = [
            test_folder.path().join("p"),
            test_folder.path().join("p").join("48"),
            test_folder.path().join("parts"),
            test_folder.path().join("parts").join("s"),
            test_folder.path().join("extra"),
        ];
        for path in &paths {
            std::fs::create_dir(path).unwrap();
        }
        let mut resolver = DiskResolver::new_from_catalog(test_folder.path()).unwrap();
        resolver.add_path(test_folder.path().join("extra")).unwrap();
        assert_eq!(5, resolver.base_paths.len());
        for path in &paths {
            assert!(resolver
                .base_paths
                .iter()
                .find(|&p| { p == path })
                .is_some());
        }
    }

    #[test]
    fn test_disk_resolver_resolve_path() {
        let test_folder = testutils::setup_test_folder("disk_resolver");
        let base_path = test_folder.path().join("parts");
        std::fs::create_dir(&base_path).unwrap_or_default();

        // Create disk-based resolver
        let resolver = DiskResolver::new_from_catalog(test_folder.path()).unwrap();

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
    }

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

    #[test]
    fn test_simple_logger() {
        let logger = SimpleLogger::new();

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
