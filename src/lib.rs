//! # weldr
//!
//! weldr is a Rust library to manipulate [LDraw](https://www.ldraw.org/) files
//! ([format specification](https://www.ldraw.org/article/218.html)), which are files describing
//! 3D models of [LEGO®](http://www.lego.com)* pieces.
//!
//! weldr allows building command-line tools and applications leveraging
//! [the fantastic database of pieces](https://www.ldraw.org/cgi-bin/ptlist.cgi) contributed by
//! the LDraw community.
//!
//! ## Example
//!
//! Parse a single `.ldr` line containing a [file reference command (line type 1)](https://www.ldraw.org/article/218.html#lt1):
//!
//! ```rust
//! extern crate weldr;
//!
//! use weldr::read_lines;
//!
//! fn main() {}
//!
//! #[test]
//! fn parse_file_ref() {
//!   let ldr = b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 s/6143.dat";
//!   let data = read_lines(ldr);
//!   let res = CommandType::SubFileRef(SubFileRefCmd{
//!     color: 16,
//!     pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
//!     row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
//!     row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
//!     row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
//!     file: "s/6143.dat"
//!   });
//!   assert_eq!(data, Ok((&b""[..], vec![res])));
//! }
//! ```
//!
//! The `read_lines()` function can be used to parse an entire file too.
//!
//! The code is available on [GitHub](https://github.com/djeedai/weldr).
//!
//! ## Technical features
//!
//! weldr leverages the [nom parser combinator library](https://crates.io/crates/nom) to efficiently
//! and reliably parse LDraw files, and transform them into in-memory data structures for consumption.
//! All parsing is done on `&[u8]` input expected to contain [specification](https://www.ldraw.org/article/218.html)-compliant
//! LDraw content. In particular, this means:
//!
//! - UTF-8 encoded input
//! - Both DOS/Windows `<CR><LF>` and Unix `<LF>` line termination accepted
//!
//! ## Copyrights
//!
//! The current code repository is licensed under the MIT license.
//!
//! LDraw™ is a trademark owned and licensed by the Estate of James Jessiman, which does not sponsor, endorse, or authorize this project.
//!
//! *LEGO® is a registered trademark of the LEGO Group, which does not sponsor, endorse, or authorize this project.

// TEMP
#![allow(unused_imports)]
#![allow(dead_code)]

#[macro_use]
extern crate nom;

#[cfg(feature = "cgmath")]
extern crate cgmath;

use std::{
  str, str::FromStr, str::from_utf8,
  collections::HashMap,
  rc::Rc,
  cell::RefCell
};
use nom::{
  bytes::complete::{take_while1, take_until},
  character::{
    is_alphabetic, is_alphanumeric, is_digit,
    complete::{alphanumeric1 as alphanumeric, line_ending as eol, digit1},
  },
  number::complete::float,
  multi::many0,
  sequence::terminated,
  IResult,
};

#[cfg(not(feature = "cgmath"))]
mod math;

#[cfg(not(feature = "cgmath"))]
use math::Vec3;

#[cfg(feature = "cgmath")]
type Vec3 = cgmath::Vector3<f32>;

// LDraw File Format Specification
// https://www.ldraw.org/article/218.html

// "Whitespace is defined as one or more spaces (#32), tabs (#9), or combination thereof."
fn is_space(chr: u8) -> bool {
  chr == b'\t' || chr == b' '
}

named!(take_spaces,
  take_while!(is_space)
);

// "All lines in the file must use the standard DOS/Windows line termination of <CR><LF>
// (carriage return/line feed). The file is permitted (but not required) to end with a <CR><LF>.
// It is recommended that all LDraw-compliant programs also be capable of reading files with the
// standard Unix line termination of <LF> (line feed)."
fn end_of_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
  if input.is_empty() {
    Ok((input, input))
  } else {
    eol(input)
  }
}

// Detect a *potential* end of line <CR><LF> or <LF> by testing for either of <CR>
// and <LF>. Note that this doesn't necessarily means a proper end of line if <CR>
// is not followed by <LF>, but we assume this doesn't happen.
#[inline]
pub fn is_cr_or_lf(chr: u8) -> bool {
  chr == b'\n' || chr == b'\r'
}

// Parse any character which is not <CR> or <LF>, potentially until the end of input.
fn take_not_cr_or_lf(input: &[u8]) -> IResult<&[u8], &[u8]> {
  input.split_at_position_complete(|item| is_cr_or_lf(item))
}

// Parse a single comma ',' character.
fn single_comma(input: &[u8]) -> IResult<&[u8], &[u8]> {
  if !input.is_empty() && (input[0] == b',') {
    Ok((&input[1..], &input[..1]))
  } else {
    // To work with separated_list!(), must return an Err::Error
    // when the separator doesn't parse anymore (and therefore
    // the list ends).
    Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)))
  }
}

// Parse any character which is not a comma ',' or <CR> or <LF>, potentially until the end of input.
fn take_not_comma_or_eol(input: &[u8]) -> IResult<&[u8], &[u8]> {
  input.split_at_position_complete(|item| item == b',' || is_cr_or_lf(item))
}

// Read the command ID and swallow the following space
fn read_cmd_id_str(input: &[u8]) -> IResult<&[u8], &[u8]> {
  terminated(take_while1(is_digit), sp)(input)
}

named!(category<CommandType>,
  do_parse!(
    tag!(b"!CATEGORY") >>
    sp >>
    content: take_not_cr_or_lf >> (
      CommandType::Category(CategoryCmd{ category: std::str::from_utf8(content).unwrap().to_string() })
    )
  )
);

named!(keywords_list<Vec<&[u8]>>,
  separated_nonempty_list!(
    single_comma,
    take_not_comma_or_eol
  )
);

named!(keywords<CommandType>,
  do_parse!(
    tag!(b"!KEYWORDS") >>
    sp >>
    keywords: keywords_list >> (
      CommandType::Keywords(KeywordsCmd{ keywords: keywords.iter().map(|&kw| std::str::from_utf8(kw).unwrap().trim().to_string()).collect() })
    )
  )
);

named!(comment<CommandType>,
  do_parse!(
    content: take_not_cr_or_lf >> (
      CommandType::Comment(CommentCmd{ text: std::str::from_utf8(content).unwrap().to_string() })
    )
  )
);

named!(meta_cmd<CommandType>,
  alt!(category | keywords | comment)
);

named!(pub sp<char>, char!(' '));

named!(read_vec3<Vec3>,
  do_parse!(
    x: float >>
    sp >>
    y: float >>
    sp >>
    z: float >> (
      Vec3 {
        x: x,
        y: y,
        z: z
      }
    )
  )
);

named!(color_id<i32>,
  map_res!(
    map_res!(digit1, str::from_utf8),
    str::parse::<i32>
  )
);

#[inline]
fn is_filename_char(chr: u8) -> bool {
  is_alphanumeric(chr) || chr == b'/' || chr == b'\\' || chr == b'.' || chr == b'-'
}

use nom::InputTakeAtPosition;
use nom::error::ErrorKind;

fn filename_char(input: &[u8]) -> IResult<&[u8], &[u8]> {
  // TODO - Split at EOL instead and accept all characters for filename?
  input.split_at_position1_complete(|item| !is_filename_char(item), ErrorKind::AlphaNumeric)
}

named!(filename<&str>,
  map_res!(
    filename_char,
    str::from_utf8
  )
);

named!(file_ref_cmd<CommandType>,
do_parse!(
  color: color_id >>
  sp >>
  pos: read_vec3 >>
  sp >>
  row0: read_vec3 >>
  sp >>
  row1: read_vec3 >>
  sp >>
  row2: read_vec3 >>
  sp >>
  file: filename >> (
    CommandType::SubFileRef(SubFileRefCmd{
      color: color,
      pos: pos,
      row0: row0,
      row1: row1,
      row2: row2,
      file: SubFileRef::UnresolvedRef(file.to_string())
    })
  )
)
);

named!(line_cmd<CommandType>,
  do_parse!(
    color: color_id >>
    sp >>
    v1: read_vec3 >>
    sp >>
    v2: read_vec3 >> (
      CommandType::Line(LineCmd{
        color: color,
        vertices: [v1, v2]
      })
    )
  )
);

named!(tri_cmd<CommandType>,
  do_parse!(
    color: color_id >>
    sp >>
    v1: read_vec3 >>
    sp >>
    v2: read_vec3 >>
    sp >>
    v3: read_vec3 >> (
      CommandType::Triangle(TriangleCmd{
        color: color,
        vertices: [v1, v2, v3]
      })
    )
  )
);

named!(quad_cmd<CommandType>,
  do_parse!(
    color: color_id >>
    sp >>
    v1: read_vec3 >>
    sp >>
    v2: read_vec3 >>
    sp >>
    v3: read_vec3 >>
    sp >>
    v4: read_vec3 >> (
      CommandType::Quad(QuadCmd{
        color: color,
        vertices: [v1, v2, v3, v4]
      })
    )
  )
);

named!(opt_line_cmd<CommandType>,
  do_parse!(
    color: color_id >>
    sp >>
    v1: read_vec3 >>
    sp >>
    v2: read_vec3 >>
    sp >>
    v3: read_vec3 >>
    sp >>
    v4: read_vec3 >> (
      CommandType::OptLine(OptLineCmd{
        color: color,
        vertices: [v1, v2],
        control_points: [v3, v4]
      })
    )
  )
);

// Zero or more "spaces", as defined in LDraw standard.
// Valid even on empty input.
fn space0(input: &[u8]) -> IResult<&[u8], &[u8]> {
  input.split_at_position_complete(|item| !is_space(item))
}

// Zero or more "spaces", as defined in LDraw standard.
// Valid even on empty input.
fn space_or_eol0(input: &[u8]) -> IResult<&[u8], &[u8]> {
  input.split_at_position_complete(|item| !is_space(item) && !is_cr_or_lf(item))
}

// An empty line made of optional spaces, and ending with an end-of-line sequence
// (either <CR><LF> or <LF> alone) or the end of input.
// Valid even on empty input.
named!(empty_line,
  terminated!(space0, end_of_line)
);

// "There is no line length restriction. Each command consists of optional leading
// whitespace followed by whitespace-delimited tokens. Some commands also have trailing
// arbitrary data which may itself include internal whitespace; such data is not tokenized,
// but treated as single unit according to the command."
//
// "Lines may also be empty or consist only of whitespace. Such lines have no effect."
//
// "The line type of a line is the first number on the line."
// "If the line type of the command is invalid, the line is ignored."
named!(pub read_line<CommandType>,
  do_parse!(
    space_or_eol0 >>
    cmd: switch!(read_cmd_id_str,
        b"0" => call!(meta_cmd) |
        b"1" => call!(file_ref_cmd) |
        b"2" => call!(line_cmd) |
        b"3" => call!(tri_cmd) |
        b"4" => call!(quad_cmd) |
        b"5" => call!(opt_line_cmd)
    ) >> 
    end_of_line >> (cmd)
  )
);

// "An LDraw file consists of one command per line."
named!(pub read_lines<Vec<CommandType>>,
  many0!(read_line)
);

struct QueuedFileRef {
  /// Filename of unresolved source file.
  filename: String,
  /// Referer source file which requested the resolution.
  referer: Rc<RefCell<SourceFile>>
}

use std::collections::HashSet;

struct ResolveQueue {
  /// Queue of pending items to resolve and load.
  queue: Vec<QueuedFileRef>,
  /// Number of pending items in the queue for each filename.
  pending_count: HashMap<String, u32>
}

impl ResolveQueue {
  fn new() -> ResolveQueue {
    return ResolveQueue{ queue: vec![], pending_count: HashMap::new() }
  }

  fn push(&mut self, filename: &str, referer_filename: &str, referer: Rc<RefCell<SourceFile>>) {
    if let Some(num_pending) = self.pending_count.get_mut(referer_filename) {
      assert!(*num_pending > 0); // should not make it to the queue if already resolved
      *num_pending += 1;
    } else {
      self.pending_count.insert(referer_filename.to_string(), 1);
    }
    self.queue.push(QueuedFileRef{ filename: filename.to_string(), referer });
  }

  fn pop(&mut self) -> Option<(QueuedFileRef, u32)> {
    match self.queue.pop() {
      Some(qfr) => {
        let num_pending = self.pending_count.get_mut(&qfr.referer.borrow().filename).unwrap();
        *num_pending -= 1;
        Some((qfr, *num_pending))
      },
      None => None
    }
  }

  fn reset(&mut self) {
    self.queue.clear();
    self.pending_count.clear();
  }
}

fn resolve_file_refs(source_file: Rc<RefCell<SourceFile>>, queue: &mut ResolveQueue, source_map: &mut HashMap<String, Rc<RefCell<SourceFile>>>) {
  let referer_filename = source_file.borrow().filename.clone();
  let mut subfile_set = HashSet::new();
  for cmd in &mut source_file.borrow_mut().cmds {
    if let CommandType::SubFileRef(sfr_cmd) = cmd {
      // All subfile refs come out of read_lines() as unresolved by definition,
      // since read_lines() doesn't have access to the source map nor the resolver.
      // See if we can resolve some of them now.
      if let SubFileRef::UnresolvedRef(subfilename) = &sfr_cmd.file {
        // Check the source map
        let subfilename = subfilename.to_string();
        if let Some(existing_source_file) = source_map.get(&subfilename) {
          // If already parsed, reuse
          println!("Updating resolved subfile ref in {} -> {}", referer_filename, subfilename);
          sfr_cmd.file = SubFileRef::ResolvedRef(Rc::clone(existing_source_file));
        } else if subfile_set.contains(&subfilename) {
          println!("Ignoring already-queued unresolved subfile ref in {} -> {}", referer_filename, subfilename);
        } else {
          // If not, push to queue for later parsing, but only once
          println!("Queuing unresolved subfile ref in {} -> {}", referer_filename, subfilename);
          queue.push(&subfilename[..], &referer_filename[..], source_file.clone());
          subfile_set.insert(subfilename);
        }
      }
    }
  }
}

fn load_and_parse_single_file(filename: &str, resolver: &dyn FileRefResolver) -> SourceFile {
  //match resolver.resolve(filename) {}
  let raw_content = resolver.resolve(filename);
  let mut source_file = SourceFile{
    filename: filename.to_string(),
    raw_content,
    cmds: Vec::new()
  };
  match read_lines(&source_file.raw_content[..]) {
    Ok((_, cmds)) => {
      source_file.cmds = cmds;
      source_file
    },
    Err(_) => panic!("TODO : handle parsing error")
  }
}

/// Parse a single file using the given resolver and source map.
///
/// Attempt to load the content of `filename` via the given `resolver`, and parse it.
/// Then recursiverly look for sub-file commands inside that root file, and try to resolve
/// the content of those sub-files and parse them too. All the loaded and parsed files end
/// up populating the given `source_map`, which can be pre-populated manually or from a
/// previous call with already loaded and parsed files.
/// ```rust
/// use weldr::{ FileRefResolver, parse };
/// use std::collections::HashMap;
/// 
/// struct MyCustomResolver {};
/// 
/// impl FileRefResolver for MyCustomResolver {
///   fn resolve(&self, filename: &str) -> Vec<u8> {
///     vec![] // replace with custom impl
///   }
/// }
/// 
/// fn main() {
///   let resolver = MyCustomResolver{};
///   let mut source_map = HashMap::new();
///   let root_file = parse("root.ldr", &resolver, &mut source_map);
///   assert_eq!(root_file.borrow().filename, "root.ldr");
/// }
/// ```
pub fn parse(filename: &str, resolver: &dyn FileRefResolver, source_map: &mut HashMap<String, Rc<RefCell<SourceFile>>>) -> Rc<RefCell<SourceFile>> {
  if let Some(existing_file) = source_map.get(filename) {
    return Rc::clone(existing_file);
  }
  let mut queue = ResolveQueue::new();
  println!("Loading root file: {}", filename);
  let root_file = load_and_parse_single_file(filename, resolver);
  let root_file = Rc::new(RefCell::new(root_file));
  {
    println!("Post-loading resolving subfile refs of root file: {}", filename);
    resolve_file_refs(Rc::clone(&root_file), &mut queue, source_map);
  }
  source_map.insert(filename.to_string(), Rc::clone(&root_file));
  while let Some(queued_file) = queue.pop() {
    let num_pending_left = queued_file.1;
    let filename = &queued_file.0.filename;
    println!("Dequeuing sub-file: {}", filename);
    if source_map.contains_key(filename) {
      println!("Already parsed; reusing sub-file: {}", filename);
    } else {
      println!("Not yet parsed; parsing sub-file: {}", filename);
      let source_file = load_and_parse_single_file(&filename[..], resolver);
      let subfile = Rc::new(RefCell::new(source_file));
      println!("Post-loading resolving subfile refs of sub-file: {}", filename);
      resolve_file_refs(Rc::clone(&subfile), &mut queue, source_map);
      source_map.insert(filename.clone(), Rc::clone(&subfile));
    }
    // Re-resolve the source file that triggered this sub-file loading to update its subfile refs
    // if there is no more sub-file references enqueued.
    if num_pending_left == 0 {
      println!("Re-resolving referer file on last resolved ref: {}", queued_file.0.referer.borrow().filename);
      resolve_file_refs(queued_file.0.referer, &mut queue, source_map);
    }
  }
  root_file
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!CATEGORY language extension](https://www.ldraw.org/article/340.html#category).
#[derive(Debug, PartialEq)]
pub struct CategoryCmd {
  /// Category name.
  category: String
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!KEYWORDS language extension](https://www.ldraw.org/article/340.html#keywords).
#[derive(Debug, PartialEq)]
pub struct KeywordsCmd {
  /// List of keywords.
  keywords: Vec<String>
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) comment.
#[derive(Debug, PartialEq)]
pub struct CommentCmd {
  /// Comment content, excluding the command identififer `0` and the optional comment marker `//`.
  text: String
}

/// Single LDraw source file loaded and optionally parsed.
#[derive(Debug, PartialEq)]
pub struct SourceFile {
  /// The relative filename of the file as resolved.
  pub filename: String,
  /// Raw UTF-8 file content (without BOM) loaded from the resolved file. Line ending can be
  /// Unix style `\n` or Windows style `\r\n`. As a convenience, parsing handles both indifferently,
  /// so the file can contain a mix of both, although this is not recommended.
  pub raw_content: Vec<u8>,
  /// LDraw commands parsed from the raw text content of the file.
  pub cmds: Vec<CommandType>
}

#[derive(Debug, PartialEq)]
pub enum SubFileRef {
  ResolvedRef(Rc<RefCell<SourceFile>>),
  UnresolvedRef(String)
}

/// Line Type 1 LDraw command to reference a sub-file from the current file.
/// 
/// [Specification](https://www.ldraw.org/article/218.html#lt1)
#[derive(Debug, PartialEq)]
pub struct SubFileRefCmd {
  /// Color code of the part.
  pub color: i32,
  /// Position.
  pub pos: Vec3,
  /// First row of rotation+scaling matrix part.
  pub row0: Vec3,
  /// Second row of rotation+scaling matrix part.
  pub row1: Vec3,
  /// Third row of rotation+scaling matrix part.
  pub row2: Vec3,
  /// Referenced sub-file.
  pub file: SubFileRef
}

/// Line Type 2 LDraw command to draw a segment between 2 vertices.
/// 
/// [Specification](https://www.ldraw.org/article/218.html#lt2)
#[derive(Debug, PartialEq)]
pub struct LineCmd {
  /// Color code of the primitive.
  pub color: i32,
  /// Vertices of the segment.
  pub vertices: [Vec3; 2]
}

/// Line Type 3 LDraw command to draw a triangle between 3 vertices.
/// 
/// [Specification](https://www.ldraw.org/article/218.html#lt3)
#[derive(Debug, PartialEq)]
pub struct TriangleCmd {
  /// Color code of the primitive.
  pub color: i32,
  /// Vertices of the triangle.
  pub vertices: [Vec3; 3]
}

/// Line Type 4 LDraw command to draw a quad between 4 vertices.
/// 
/// [Specification](https://www.ldraw.org/article/218.html#lt4)
#[derive(Debug, PartialEq)]
pub struct QuadCmd {
  /// Color code of the primitive.
  pub color: i32,
  /// Vertices of the quad.
  pub vertices: [Vec3; 4]
}

/// Line Type 5 LDraw command to draw an optional segment between two vertices,
/// aided by 2 control points.
/// 
/// [Specification](https://www.ldraw.org/article/218.html#lt5)
#[derive(Debug, PartialEq)]
pub struct OptLineCmd {
  /// Color code of the primitive.
  pub color: i32,
  /// Vertices of the segment.
  pub vertices: [Vec3; 2],
  /// Control points of the segment.
  pub control_points: [Vec3; 2]
}

/// Types of commands contained in a LDraw file.
#[derive(Debug, PartialEq)]
pub enum CommandType {
  /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
  /// [!CATEGORY language extension](https://www.ldraw.org/article/340.html#category).
  Category(CategoryCmd),
  /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
  /// [!KEYWORDS language extension](https://www.ldraw.org/article/340.html#keywords).
  Keywords(KeywordsCmd),
  /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) comment.
  /// Note: any line type 0 not otherwise parsed as a known meta-command is parsed as a generic comment.
  Comment(CommentCmd),
  /// [Line Type 1](https://www.ldraw.org/article/218.html#lt1) sub-file reference.
  SubFileRef(SubFileRefCmd),
  /// [Line Type 2](https://www.ldraw.org/article/218.html#lt2) segment.
  Line(LineCmd),
  /// [Line Type 3](https://www.ldraw.org/article/218.html#lt3) triangle.
  Triangle(TriangleCmd),
  /// [Line Type 4](https://www.ldraw.org/article/218.html#lt4) quadrilateral.
  Quad(QuadCmd),
  /// [Line Type 5](https://www.ldraw.org/article/218.html#lt5) optional line.
  OptLine(OptLineCmd)
}

/// Resolver trait for sub-file references ([Line Type 1](https://www.ldraw.org/article/218.html#lt1) LDraw command).
/// 
/// An implementation of this trait must be passed to [`parse()`](weldr::parse) to allow resolving sub-file
/// references recursively, and parsing all dependent sub-files of the top-level file being parsed. Implementations
/// are free to decide how to retrieve the file, but must ensure that all canonical paths are in scope, as sub-file
/// references can be relative to any of those:
/// - `/p/`       - Parts primitives
/// - `/p/48/`    - High-resolution primitives
/// - `/parts/`   - Main catalog of parts
/// - `/parts/s/` - Catalog of sub-parts commonly used
pub trait FileRefResolver {
  /// Resolve the given file reference `filename`, given as it appears in a sub-file reference, and return
  /// the content of the file as a UTF-8 encoded buffer of bytes, without BOM. Line ending can be indifferently
  /// Unix style `\n` or Windows style `\r\n`.
  /// 
  /// See [`parse()`](weldr::parse) for usage.
  fn resolve(&self, filename: &str) -> Vec<u8>;
}

#[cfg(test)]
mod tests {

  use super::*;

  #[test]
  fn test_color_id() {
    assert_eq!(color_id(b"16 "), Ok((&b" "[..], 16)));
  }

  #[test]
  fn test_vec3() {
    assert_eq!(read_vec3(b"0 0 0"), Ok((&b""[..], Vec3 { x: 0., y: 0., z: 0. })));
    assert_eq!(read_vec3(b"0 0 0 1"), Ok((&b" 1"[..], Vec3 { x: 0., y: 0., z: 0. })));
    assert_eq!(read_vec3(b"2 5 -7"), Ok((&b""[..], Vec3 { x: 2., y: 5., z: -7. })));
    assert_eq!(read_vec3(b"2.3 5 -7.4"), Ok((&b""[..], Vec3 { x: 2.3, y: 5., z: -7.4 })));
  }

  #[test]
  fn test_end_of_line() {
    assert_eq!(end_of_line(b""), Ok((&b""[..], &b""[..])));
    assert_eq!(end_of_line(b"\n"), Ok((&b""[..], &b"\n"[..])));
    assert_eq!(end_of_line(b"\r\n"), Ok((&b""[..], &b"\r\n"[..])));
  }

  #[test]
  fn test_take_not_cr_or_lf() {
    assert_eq!(take_not_cr_or_lf(b""), Ok((&b""[..], &b""[..])));
    assert_eq!(take_not_cr_or_lf(b"\n"), Ok((&b"\n"[..], &b""[..])));
    assert_eq!(take_not_cr_or_lf(b"\r\n"), Ok((&b"\r\n"[..], &b""[..])));
    assert_eq!(take_not_cr_or_lf(b"\n\n\n"), Ok((&b"\n\n\n"[..], &b""[..])));
    assert_eq!(take_not_cr_or_lf(b"\r\n\r\n\r\n"), Ok((&b"\r\n\r\n\r\n"[..], &b""[..])));
    assert_eq!(take_not_cr_or_lf(b" a \n"), Ok((&b"\n"[..], &b" a "[..])));
    assert_eq!(take_not_cr_or_lf(b"test"), Ok((&b""[..], &b"test"[..])));
  }

  use nom::{Err, Needed};
  use nom::error::ErrorKind;

  #[test]
  fn test_single_comma() {
    assert_eq!(single_comma(b""), Err(Err::Error((&b""[..], ErrorKind::Tag))));
    assert_eq!(single_comma(b","), Ok((&b""[..], &b","[..])));
    assert_eq!(single_comma(b",s"), Ok((&b"s"[..], &b","[..])));
    assert_eq!(single_comma(b"w,s"), Err(Err::Error((&b"w,s"[..], ErrorKind::Tag))));
  }

  #[test]
  fn test_keywords_list() {
    assert_eq!(keywords_list(b""), Err(Err::Error((&b""[..], ErrorKind::SeparatedList))));
    assert_eq!(keywords_list(b"a"), Ok((&b""[..], vec![&b"a"[..]])));
    assert_eq!(keywords_list(b"a,b,c"), Ok((&b""[..], vec![&b"a"[..], &b"b"[..], &b"c"[..]])));
  }

  #[test]
  fn test_filename_char() {
    assert_eq!(filename_char(b""), Err(Err::Error((&b""[..], ErrorKind::AlphaNumeric))));
    assert_eq!(filename_char(b"a"), Ok((&b""[..], &b"a"[..])));
    assert_eq!(filename_char(b"a-"), Ok((&b""[..], &b"a-"[..])));
    assert_eq!(filename_char(b"a/sad.bak\\ww.dat"), Ok((&b""[..], &b"a/sad.bak\\ww.dat"[..])));
  }

  #[test]
  fn test_filename() {
    assert_eq!(filename(b"asd\\kw/l.ldr"), Ok((&b""[..], "asd\\kw/l.ldr")));
    assert_eq!(filename(b"asdkwl.ldr"), Ok((&b""[..], "asdkwl.ldr")));
    assert_eq!(filename(b"asd\\kw/l.ldr\n"), Ok((&b"\n"[..], "asd\\kw/l.ldr")));
    assert_eq!(filename(b"asdkwl.ldr\n"), Ok((&b"\n"[..], "asdkwl.ldr")));
    assert_eq!(filename(b"asd\\kw/l.ldr\r\n"), Ok((&b"\r\n"[..], "asd\\kw/l.ldr")));
    assert_eq!(filename(b"asdkwl.ldr\r\n"), Ok((&b"\r\n"[..], "asdkwl.ldr")));
  }

  #[test]
  fn test_category_cmd() {
    let res = CommandType::Category(CategoryCmd{ category: "Figure Accessory".to_string() });
    assert_eq!(category(b"!CATEGORY Figure Accessory"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_keywords_cmd() {
    let res = CommandType::Keywords(KeywordsCmd{ keywords: vec![
      "western".to_string(),
      "wild west".to_string(),
      "spaghetti western".to_string(),
      "horse opera".to_string(),
      "cowboy".to_string()
    ] });
    assert_eq!(keywords(b"!KEYWORDS western, wild west, spaghetti western, horse opera, cowboy"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_comment_cmd() {
    let comment = b"test of comment, with \"weird\" characters";
    let res = CommandType::Comment(CommentCmd{ text: std::str::from_utf8(comment).unwrap().to_string() });
    assert_eq!(meta_cmd(comment), Ok((&b""[..], res)));
  }

  #[test]
  fn test_file_ref_cmd() {
    let res = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: SubFileRef::UnresolvedRef("aaaaaaddd".to_string())
    });
    assert_eq!(file_ref_cmd(b"16 0 0 0 1 0 0 0 1 0 0 0 1 aaaaaaddd"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_space0() {
    assert_eq!(space0(b""), Ok((&b""[..], &b""[..])));
    assert_eq!(space0(b" "), Ok((&b""[..], &b" "[..])));
    assert_eq!(space0(b"   "), Ok((&b""[..], &b"   "[..])));
    assert_eq!(space0(b"  a"), Ok((&b"a"[..], &b"  "[..])));
    assert_eq!(space0(b"a  "), Ok((&b"a  "[..], &b""[..])));
  }

  #[test]
  fn test_space_or_eol0() {
    assert_eq!(space_or_eol0(b""), Ok((&b""[..], &b""[..])));
    assert_eq!(space_or_eol0(b" "), Ok((&b""[..], &b" "[..])));
    assert_eq!(space_or_eol0(b"   "), Ok((&b""[..], &b"   "[..])));
    assert_eq!(space_or_eol0(b"  a"), Ok((&b"a"[..], &b"  "[..])));
    assert_eq!(space_or_eol0(b"a  "), Ok((&b"a  "[..], &b""[..])));
    assert_eq!(space_or_eol0(b"\n"), Ok((&b""[..], &b"\n"[..])));
    assert_eq!(space_or_eol0(b"\n\n\n"), Ok((&b""[..], &b"\n\n\n"[..])));
    assert_eq!(space_or_eol0(b"\n\r\n"), Ok((&b""[..], &b"\n\r\n"[..])));
    // Unfortunately <LF> alone is not handled well, but we assume this needs to be ignored too
    assert_eq!(space_or_eol0(b"\n\r\r\r\n"), Ok((&b""[..], &b"\n\r\r\r\n"[..])));
    assert_eq!(space_or_eol0(b"  \n"), Ok((&b""[..], &b"  \n"[..])));
    assert_eq!(space_or_eol0(b"  \n   "), Ok((&b""[..], &b"  \n   "[..])));
    assert_eq!(space_or_eol0(b"  \n   \r\n"), Ok((&b""[..], &b"  \n   \r\n"[..])));
    assert_eq!(space_or_eol0(b"  \n   \r\n "), Ok((&b""[..], &b"  \n   \r\n "[..])));
    assert_eq!(space_or_eol0(b"  \nsa"), Ok((&b"sa"[..], &b"  \n"[..])));
    assert_eq!(space_or_eol0(b"  \n  \r\nsa"), Ok((&b"sa"[..], &b"  \n  \r\n"[..])));
  }

  #[test]
  fn test_empty_line() {
    assert_eq!(empty_line(b""), Ok((&b""[..], &b""[..])));
    assert_eq!(empty_line(b" "), Ok((&b""[..], &b" "[..])));
    assert_eq!(empty_line(b"   "), Ok((&b""[..], &b"   "[..])));
    assert_eq!(empty_line(b"  a"), Err(Err::Error((&b"a"[..], ErrorKind::CrLf))));
    assert_eq!(empty_line(b"a  "), Err(Err::Error((&b"a  "[..], ErrorKind::CrLf))));
  }

  #[test]
  fn test_read_cmd() {
    let res = CommandType::Comment(CommentCmd{ text: "this doesn't matter".to_string() });
    assert_eq!(read_line(b"0 this doesn't matter"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_line_cmd() {
    let res = CommandType::Line(LineCmd{ color: 16, vertices: [
      Vec3{ x: 1.0, y: 1.0, z: 0.0 },
      Vec3{ x: 0.9239, y: 1.0, z: 0.3827 }
    ] });
    assert_eq!(read_line(b"2 16 1 1 0 0.9239 1 0.3827"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_tri_cmd() {
    let res = CommandType::Triangle(TriangleCmd{ color: 16, vertices: [
      Vec3{ x: 1.0, y: 1.0, z: 0.0 },
      Vec3{ x: 0.9239, y: 1.0, z: 0.3827 },
      Vec3{ x: 0.9239, y: 0.0, z: 0.3827 }
    ] });
    assert_eq!(read_line(b"3 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_quad_cmd() {
    let res = CommandType::Quad(QuadCmd{ color: 16, vertices: [
      Vec3{ x: 1.0, y: 1.0, z: 0.0 },
      Vec3{ x: 0.9239, y: 1.0, z: 0.3827 },
      Vec3{ x: 0.9239, y: 0.0, z: 0.3827 },
      Vec3{ x: 1.0, y: 0.0, z: 0.0 }
    ] });
    assert_eq!(read_line(b"4 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_opt_line_cmd() {
    let res = CommandType::OptLine(OptLineCmd{ color: 16, vertices: [
      Vec3{ x: 1.0, y: 1.0, z: 0.0 },
      Vec3{ x: 0.9239, y: 1.0, z: 0.3827 }
    ], control_points: [
      Vec3{ x: 0.9239, y: 0.0, z: 0.3827 },
      Vec3{ x: 1.0, y: 0.0, z: 0.0 }
    ] });
    assert_eq!(read_line(b"5 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_line_subfileref() {
    let res = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string())
    });
    assert_eq!(read_line(b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_lines() {
    let cmd0 = CommandType::Comment(CommentCmd{ text: "this doesn't matter".to_string() });
    let cmd1 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string())
    });
    assert_eq!(read_lines(b"\n0 this doesn't matter\n\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"), Ok((&b""[..], vec![cmd0, cmd1])));
    
    let cmd0 = CommandType::Comment(CommentCmd{ text: "this doesn't \"matter\"".to_string() });
    let cmd1 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string())
    });
    assert_eq!(read_lines(b"\r\n0 this doesn't \"matter\"\r\n\r\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd\n"), Ok((&b""[..], vec![cmd0, cmd1])));

    let cmd0 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string())
    });
    let cmd1 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string())
    });
    assert_eq!(read_lines(b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"), Ok((&b""[..], vec![cmd0, cmd1])));
  }
}
