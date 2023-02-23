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
//! Parse a single LDraw file containing 2 commands:
//! - A comment : "this is a comment"
//! - A segment command to draw a segment between 2 vertices
//!
//! ```rust
//! extern crate weldr;
//!
//! use weldr::{parse_raw, Command, CommentCmd, LineCmd, Vec3};
//!
//! fn main() {}
//!
//! #[test]
//! fn parse_ldr() {
//!   let ldr = b"0 this is a comment\n2 16 0 0 0 1 1 1";
//!   let cmds = parse_raw(ldr);
//!   let cmd0 = Command::Comment(CommentCmd::new("this is a comment"));
//!   let cmd1 = Command::Line(LineCmd{
//!     color: 16,
//!     vertices: [
//!       Vec3{ x: 0.0, y: 0.0, z: 0.0 },
//!       Vec3{ x: 1.0, y: 1.0, z: 1.0 }
//!     ]
//!   });
//!   assert_eq!(cmds, vec![cmd0, cmd1]);
//! }
//! ```
//!
//! A slightly more involved but more powerful approach is to load and resolve a file and all its
//! sub-file references recursively using the [`parse()`] function. This requires implementing the
//! [`FileRefResolver`] trait to load file content by reference filename.
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
#![allow(dead_code)]

#[macro_use]
extern crate log;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while, take_while1, take_while_m_n},
    character::{
        complete::{digit1, line_ending as eol},
        is_alphanumeric, is_digit,
    },
    combinator::{complete, map, map_res, opt},
    error::ErrorKind,
    multi::{many0, separated_list1},
    number::complete::float,
    sequence::{terminated, tuple},
    IResult, InputTakeAtPosition,
};
use std::{
    collections::{HashMap, HashSet},
    str,
};

pub type Vec3 = cgmath::Vector3<f32>;
pub type Vec4 = cgmath::Vector4<f32>;
pub type Mat4 = cgmath::Matrix4<f32>;

#[cfg(feature = "cgmath")]
pub type Vec4 = cgmath::Vector4<f32>;

#[cfg(feature = "cgmath")]
pub type Mat4 = cgmath::Matrix4<f32>;

pub mod error;

pub use error::{Error, ParseError, ResolveError};

// LDraw File Format Specification
// https://www.ldraw.org/article/218.html

fn nom_error(i: &[u8], kind: ErrorKind) -> nom::Err<nom::error::Error<&[u8]>> {
    nom::Err::Error(nom::error::Error::new(i, kind))
}

// "Whitespace is defined as one or more spaces (#32), tabs (#9), or combination thereof."
fn is_space(chr: u8) -> bool {
    chr == b'\t' || chr == b' '
}

fn take_spaces(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(is_space)(i)
}

// "All lines in the file must use the standard DOS/Windows line termination of <CR><LF>
// (carriage return/line feed). The file is permitted (but not required) to end with a <CR><LF>.
// It is recommended that all LDraw-compliant programs also be capable of reading files with the
// standard Unix line termination of <LF> (line feed)."
fn end_of_line(i: &[u8]) -> IResult<&[u8], &[u8]> {
    if i.is_empty() {
        Ok((i, i))
    } else {
        eol(i)
    }
}

// Detect a *potential* end of line <CR><LF> or <LF> by testing for either of <CR>
// and <LF>. Note that this doesn't necessarily means a proper end of line if <CR>
// is not followed by <LF>, but we assume this doesn't happen.
#[inline]
fn is_cr_or_lf(chr: u8) -> bool {
    chr == b'\n' || chr == b'\r'
}

// Parse any character which is not <CR> or <LF>, potentially until the end of input.
fn take_not_cr_or_lf(i: &[u8]) -> IResult<&[u8], &[u8]> {
    i.split_at_position_complete(|item| is_cr_or_lf(item))
}

// Parse a single comma ',' character.
fn single_comma(i: &[u8]) -> IResult<&[u8], &[u8]> {
    if !i.is_empty() && (i[0] == b',') {
        Ok((&i[1..], &i[..1]))
    } else {
        // To work with separated_list!(), must return an Err::Error
        // when the separator doesn't parse anymore (and therefore
        // the list ends).
        Err(nom_error(i, nom::error::ErrorKind::Tag))
    }
}

// Parse any character which is not a comma ',' or <CR> or <LF>, potentially until the end of input.
// Invalid on empty input.
fn take_not_comma_or_eol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|item| item != b',' && !is_cr_or_lf(item))(i)
}

// Parse any character which is not a space, potentially until the end of input.
fn take_not_space(i: &[u8]) -> IResult<&[u8], &[u8]> {
    i.split_at_position_complete(|item| is_space(item))
}

// Read the command ID and swallow the following space, if any.
fn read_cmd_id_str(i: &[u8]) -> IResult<&[u8], &[u8]> {
    //terminated(take_while1(is_digit), sp)(i) //< This does not work if there's no space (e.g. 4-4cylo.dat)
    let (i, o) = i.split_at_position1_complete(|item| !is_digit(item), ErrorKind::Digit)?;
    let (i, _) = space0(i)?;
    Ok((i, o))
}

fn category(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, _) = tag(b"!CATEGORY")(i)?;
    let (i, _) = sp(i)?;
    let (i, content) = map_res(take_not_cr_or_lf, str::from_utf8)(i)?;

    Ok((
        i,
        Command::Category(CategoryCmd {
            category: content.to_string(),
        }),
    ))
}

fn keywords_list(i: &[u8]) -> IResult<&[u8], Vec<&str>> {
    separated_list1(single_comma, map_res(take_not_comma_or_eol, str::from_utf8))(i)
}

fn keywords(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, (_, _, keywords)) = tuple((tag(b"!KEYWORDS"), sp, keywords_list))(i)?;
    Ok((
        i,
        Command::Keywords(KeywordsCmd {
            keywords: keywords.iter().map(|kw| kw.trim().to_string()).collect(),
        }),
    ))
}

/// RGB color in sRGB color space.
#[derive(Debug, PartialEq)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

impl Color {
    /// Construct a new color instance from individual RGB components.
    pub fn new(red: u8, green: u8, blue: u8) -> Color {
        Color { red, green, blue }
    }
}

fn from_hex(i: &[u8]) -> Result<u8, nom::error::ErrorKind> {
    match std::str::from_utf8(i) {
        Ok(s) => match u8::from_str_radix(s, 16) {
            Ok(val) => Ok(val),
            Err(_) => Err(ErrorKind::AlphaNumeric),
        },
        Err(_) => Err(ErrorKind::AlphaNumeric),
    }
}

fn is_hex_digit(c: u8) -> bool {
    (c as char).is_digit(16)
}

fn hex_primary(i: &[u8]) -> IResult<&[u8], u8> {
    map_res(take_while_m_n(2, 2, is_hex_digit), from_hex)(i)
}

fn hex_color(i: &[u8]) -> IResult<&[u8], Color> {
    let (i, _) = tag(b"#")(i)?;
    let (i, (red, green, blue)) = tuple((hex_primary, hex_primary, hex_primary))(i)?;
    Ok((i, Color { red, green, blue }))
}

fn digit1_as_u8(i: &[u8]) -> IResult<&[u8], u8> {
    map_res(map_res(digit1, str::from_utf8), str::parse::<u8>)(i)
}

// ALPHA part of !COLOUR
fn colour_alpha(i: &[u8]) -> IResult<&[u8], Option<u8>> {
    opt(complete(|i| {
        let (i, _) = sp(i)?;
        let (i, _) = tag(b"ALPHA")(i)?;
        let (i, _) = sp(i)?;
        digit1_as_u8(i)
    }))(i)
}

// LUMINANCE part of !COLOUR
fn colour_luminance(i: &[u8]) -> IResult<&[u8], Option<u8>> {
    opt(complete(|i| {
        let (i, _) = sp(i)?;
        let (i, _) = tag(b"LUMINANCE")(i)?;
        let (i, _) = sp(i)?;
        digit1_as_u8(i)
    }))(i)
}

fn material_grain_size(i: &[u8]) -> IResult<&[u8], GrainSize> {
    alt((grain_size, grain_min_max_size))(i)
}

fn grain_size(i: &[u8]) -> IResult<&[u8], GrainSize> {
    // TODO: Create tagged float helper?
    let (i, (_, _, size)) = tuple((tag(b"SIZE"), sp, float))(i)?;
    Ok((i, GrainSize::Size(size)))
}

fn grain_min_max_size(i: &[u8]) -> IResult<&[u8], GrainSize> {
    let (i, (_, _, min_size)) = tuple((tag(b"MINSIZE"), sp, float))(i)?;
    let (i, _) = sp(i)?;
    let (i, (_, _, max_size)) = tuple((tag(b"MAXSIZE"), sp, float))(i)?;
    Ok((i, GrainSize::MinMaxSize((min_size, max_size))))
}

// GLITTER VALUE v [ALPHA a] [LUMINANCE l] FRACTION f VFRACTION vf (SIZE s | MINSIZE min MAXSIZE max)
fn glitter_material(i: &[u8]) -> IResult<&[u8], ColorFinish> {
    let (i, _) = tag_no_case(b"GLITTER")(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag_no_case(b"VALUE")(i)?;
    let (i, _) = sp(i)?;
    let (i, value) = hex_color(i)?;
    let (i, alpha) = colour_alpha(i)?;
    let (i, luminance) = colour_luminance(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag_no_case(b"FRACTION")(i)?;
    let (i, _) = sp(i)?;
    let (i, surface_fraction) = float(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag_no_case(b"VFRACTION")(i)?;
    let (i, _) = sp(i)?;
    let (i, volume_fraction) = float(i)?;
    let (i, _) = sp(i)?;
    let (i, size) = material_grain_size(i)?;

    Ok((
        i,
        ColorFinish::Material(MaterialFinish::Glitter(GlitterMaterial {
            value,
            alpha,
            luminance,
            surface_fraction,
            volume_fraction,
            size,
        })),
    ))
}

// SPECKLE VALUE v [ALPHA a] [LUMINANCE l] FRACTION f (SIZE s | MINSIZE min MAXSIZE max)
fn speckle_material(i: &[u8]) -> IResult<&[u8], ColorFinish> {
    let (i, _) = tag_no_case(b"SPECKLE")(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag_no_case(b"VALUE")(i)?;
    let (i, _) = sp(i)?;
    let (i, value) = hex_color(i)?;
    let (i, alpha) = colour_alpha(i)?;
    let (i, luminance) = colour_luminance(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag_no_case(b"FRACTION")(i)?;
    let (i, _) = sp(i)?;
    let (i, surface_fraction) = float(i)?;
    let (i, _) = sp(i)?;
    let (i, size) = material_grain_size(i)?;

    Ok((
        i,
        ColorFinish::Material(MaterialFinish::Speckle(SpeckleMaterial {
            value,
            alpha,
            luminance,
            surface_fraction,
            size,
        })),
    ))
}

// Other unrecognized MATERIAL definition
fn other_material(i: &[u8]) -> IResult<&[u8], ColorFinish> {
    let (i, content) = map_res(take_not_cr_or_lf, str::from_utf8)(i)?;
    let finish = content.trim().to_string();
    Ok((i, ColorFinish::Material(MaterialFinish::Other(finish))))
}

// MATERIAL finish part of !COLOUR
fn material_finish(i: &[u8]) -> IResult<&[u8], ColorFinish> {
    let (i, _) = tag_no_case(b"MATERIAL")(i)?;
    alt((glitter_material, speckle_material, other_material))(i)
}

// Finish part of !COLOUR
// TODO: Avoid having the leading space in each parser?
fn color_finish(i: &[u8]) -> IResult<&[u8], Option<ColorFinish>> {
    opt(complete(|i| {
        let (i, _) = sp(i)?;
        alt((
            map(tag_no_case(b"CHROME"), |_| ColorFinish::Chrome),
            map(tag_no_case(b"PEARLESCENT"), |_| ColorFinish::Pearlescent),
            map(tag_no_case(b"RUBBER"), |_| ColorFinish::Rubber),
            map(tag_no_case(b"MATTE_METALLIC"), |_| {
                ColorFinish::MatteMetallic
            }),
            map(tag_no_case(b"METAL"), |_| ColorFinish::Metal),
            material_finish,
        ))(i)
    }))(i)
}

// !COLOUR extension meta-command
fn meta_colour(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, _) = tag(b"!COLOUR")(i)?;
    let (i, _) = sp(i)?;
    let (i, name) = map_res(take_not_space, str::from_utf8)(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag(b"CODE")(i)?;
    let (i, _) = sp(i)?;
    let (i, code) = color_id(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag(b"VALUE")(i)?;
    let (i, _) = sp(i)?;
    let (i, value) = hex_color(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag(b"EDGE")(i)?;
    let (i, _) = sp(i)?;
    let (i, edge) = hex_color(i)?;
    let (i, alpha) = colour_alpha(i)?;
    let (i, luminance) = colour_luminance(i)?;
    let (i, finish) = color_finish(i)?;

    Ok((
        i,
        Command::Colour(ColourCmd {
            name: name.to_string(),
            code,
            value,
            edge,
            alpha,
            luminance,
            finish,
        }),
    ))
}

fn comment(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, comment) = map_res(take_not_cr_or_lf, str::from_utf8)(i)?;
    Ok((i, Command::Comment(CommentCmd::new(comment))))
}

fn meta_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    alt((
        complete(category),
        complete(keywords),
        complete(meta_colour),
        comment,
    ))(i)
}

fn read_vec3(i: &[u8]) -> IResult<&[u8], Vec3> {
    let (i, (x, _, y, _, z)) = tuple((float, sp, float, sp, float))(i)?;
    Ok((i, Vec3 { x, y, z }))
}

fn color_id(i: &[u8]) -> IResult<&[u8], u32> {
    map_res(map_res(digit1, str::from_utf8), str::parse::<u32>)(i)
}

#[inline]
fn is_filename_char(chr: u8) -> bool {
    is_alphanumeric(chr) || chr == b'/' || chr == b'\\' || chr == b'.' || chr == b'-'
}

fn filename_char(i: &[u8]) -> IResult<&[u8], &[u8]> {
    // TODO - Split at EOL instead and accept all characters for filename?
    i.split_at_position1_complete(|item| !is_filename_char(item), ErrorKind::AlphaNumeric)
}

fn filename(i: &[u8]) -> IResult<&[u8], &str> {
    map_res(filename_char, str::from_utf8)(i)
}

fn file_ref_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, color) = color_id(i)?;
    let (i, _) = sp(i)?;
    let (i, pos) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, row0) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, row1) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, row2) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, file) = filename(i)?;

    Ok((
        i,
        Command::SubFileRef(SubFileRefCmd {
            color,
            pos,
            row0,
            row1,
            row2,
            file: SubFileRef::UnresolvedRef(file.to_string()),
        }),
    ))
}

fn line_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, color) = color_id(i)?;
    let (i, _) = sp(i)?;
    let (i, v1) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v2) = read_vec3(i)?;
    let (i, _) = space0(i)?;

    Ok((
        i,
        Command::Line(LineCmd {
            color: color,
            vertices: [v1, v2],
        }),
    ))
}

fn tri_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, color) = color_id(i)?;
    let (i, _) = sp(i)?;
    let (i, v1) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v2) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v3) = read_vec3(i)?;
    let (i, _) = space0(i)?;

    Ok((
        i,
        Command::Triangle(TriangleCmd {
            color: color,
            vertices: [v1, v2, v3],
        }),
    ))
}

fn quad_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, color) = color_id(i)?;
    let (i, _) = sp(i)?;
    let (i, v1) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v2) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v3) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v4) = read_vec3(i)?;
    let (i, _) = space0(i)?;

    Ok((
        i,
        Command::Quad(QuadCmd {
            color: color,
            vertices: [v1, v2, v3, v4],
        }),
    ))
}

fn opt_line_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, color) = color_id(i)?;
    let (i, _) = sp(i)?;
    let (i, v1) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v2) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v3) = read_vec3(i)?;
    let (i, _) = sp(i)?;
    let (i, v4) = read_vec3(i)?;
    let (i, _) = space0(i)?;

    Ok((
        i,
        Command::OptLine(OptLineCmd {
            color,
            vertices: [v1, v2],
            control_points: [v3, v4],
        }),
    ))
}

// Zero or more "spaces", as defined in LDraw standard.
// Valid even on empty input.
fn space0(i: &[u8]) -> IResult<&[u8], &[u8]> {
    i.split_at_position_complete(|item| !is_space(item))
}

// One or more "spaces", as defined in LDraw standard.
// Valid even on empty input.
fn sp(i: &[u8]) -> IResult<&[u8], &[u8]> {
    i.split_at_position1_complete(|item| !is_space(item), ErrorKind::Space)
}

// Zero or more "spaces", as defined in LDraw standard.
// Valid even on empty input.
fn space_or_eol0(i: &[u8]) -> IResult<&[u8], &[u8]> {
    i.split_at_position_complete(|item| !is_space(item) && !is_cr_or_lf(item))
}

// An empty line made of optional spaces, and ending with an end-of-line sequence
// (either <CR><LF> or <LF> alone) or the end of input.
// Valid even on empty input.
fn empty_line(i: &[u8]) -> IResult<&[u8], &[u8]> {
    terminated(space0, end_of_line)(i)
}

// "There is no line length restriction. Each command consists of optional leading
// whitespace followed by whitespace-delimited tokens. Some commands also have trailing
// arbitrary data which may itself include internal whitespace; such data is not tokenized,
// but treated as single unit according to the command."
//
// "Lines may also be empty or consist only of whitespace. Such lines have no effect."
//
// "The line type of a line is the first number on the line."
// "If the line type of the command is invalid, the line is ignored."
fn read_line(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, _) = space_or_eol0(i)?;
    let (i, cmd_id) = read_cmd_id_str(i)?;
    let (i, cmd) = match cmd_id {
        b"0" => meta_cmd(i),
        b"1" => file_ref_cmd(i),
        b"2" => line_cmd(i),
        b"3" => tri_cmd(i),
        b"4" => quad_cmd(i),
        b"5" => opt_line_cmd(i),
        _ => Err(nom_error(i, ErrorKind::Switch)),
    }?;
    Ok((i, cmd))
}

/// Parse raw LDR content without sub-file resolution.
///
/// Parse the given LDR data passed in `ldr_content` and return the list of parsed commands.
/// Sub-file references (Line Type 1) are not resolved, and returned as [`SubFileRef::UnresolvedRef`].
///
/// The input LDR content must comply to the LDraw standard. In particular this means:
/// - UTF-8 encoded, without Byte Order Mark (BOM)
/// - Both DOS/Windows <CR><LF> and Unix <LF> line termination accepted
///
/// ```rust
/// use weldr::{parse_raw, Command, CommentCmd, LineCmd, Vec3};
///
/// fn main() {
///   let cmd0 = Command::Comment(CommentCmd::new("this is a comment"));
///   let cmd1 = Command::Line(LineCmd{
///     color: 16,
///     vertices: [
///       Vec3{ x: 0.0, y: 0.0, z: 0.0 },
///       Vec3{ x: 1.0, y: 1.0, z: 1.0 }
///     ]
///   });
///   assert_eq!(parse_raw(b"0 this is a comment\n2 16 0 0 0 1 1 1").unwrap(), vec![cmd0, cmd1]);
/// }
/// ```
pub fn parse_raw(ldr_content: &[u8]) -> Result<Vec<Command>, Error> {
    parse_raw_with_filename("", ldr_content)
}

fn parse_raw_with_filename(filename: &str, ldr_content: &[u8]) -> Result<Vec<Command>, Error> {
    // "An LDraw file consists of one command per line."
    many0(read_line)(ldr_content).map_or_else(
        |e| Err(Error::Parse(ParseError::new_from_nom(filename, &e))),
        |(_, cmds)| Ok(cmds),
    )
}

struct QueuedFileRef {
    /// Filename of unresolved source file.
    filename: String,
    /// Referer source file which requested the resolution.
    referer: SourceFileRef,
}

struct ResolveQueue {
    /// Queue of pending items to resolve and load.
    queue: Vec<QueuedFileRef>,
    /// Number of pending items in the queue for each filename.
    pending_count: HashMap<String, u32>,
}

/// Drawing context used when iterating over all drawing commands of a file via [`SourceFile::iter()`].
#[derive(Debug, Copy, Clone)]
pub struct DrawContext {
    /// Current transformation matrix for the drawing command. This is the accumulated transformation
    /// of all parent files.
    ///
    /// When drawing a primitive (line, triangle, quad), the actual position of vertices is obtained
    /// by transforming the local-space positions of the drawing command by this transformation matrix.
    ///
    /// ```rustc,ignore
    /// let v0 = draw_ctx.transform * cmd.vertices[0];
    /// ```
    pub transform: Mat4,

    /// Current color for substitution of color 16.
    pub color: u32,
}

/// Iterator over all drawing commands of a [`SourceFile`] and all its referenced sub-files.
///
/// Sub-file reference commands are not yielded, but instead the drawing commands of those
/// sub-files are iterated over. Comment commands are skipped.
pub struct CommandIterator<'a> {
    stack: Vec<(&'a SourceFile, usize, DrawContext)>,
    source_map: &'a SourceMap,
}

/// Iterator over all local commands of a [`SourceFile`].
///
/// Sub-file reference commands and comment commands are yielded like all other commands.
/// No command from any other file is yielded.
pub struct LocalCommandIterator<'a> {
    stack: Vec<&'a SourceFile>,
    index: usize,
    source_map: &'a SourceMap,
}

// impl std::iter::IntoIterator for SourceFile {
//     type Item = &'a Command;
//     type IntoIter = &'a CommandIterator;

//     fn into_iter(self) -> Self::IntoIter {
//         CommandIterator {
//             stack: vec![self.clone()],
//             index: 0,
//         }
//     }
// }

impl SourceFile {
    /// Return an iterator over all drawing commands, recursively stepping into sub-file references
    /// without returning the corresponding [`SubFileRefCmd`] command nor any comment command.
    pub fn iter<'a>(&'a self, source_map: &'a SourceMap) -> CommandIterator<'a> {
        let draw_ctx = DrawContext {
            transform: Mat4::from_scale(1.0),
            color: 16,
        };
        CommandIterator {
            stack: vec![(&self, 0, draw_ctx)],
            source_map,
        }
    }

    /// Return an iterator over all commands local to this source file, including sub-file references
    /// and comments. Unlike [`SourceFile::iter()`], this doesn't step into those sub-file references
    /// but remains in the local source file.
    pub fn local_iter<'a>(&'a self, source_map: &'a SourceMap) -> LocalCommandIterator<'a> {
        LocalCommandIterator {
            stack: vec![&self],
            index: 0,
            source_map,
        }
    }
}

impl<'a> Iterator for CommandIterator<'a> {
    type Item = (DrawContext, &'a Command);

    fn next(&mut self) -> Option<(DrawContext, &'a Command)> {
        while let Some(entry) = self.stack.last_mut() {
            let cmds = &entry.0.cmds;
            let index = &mut entry.1;
            let draw_ctx = &entry.2;
            if *index < cmds.len() {
                let cmd = &cmds[*index];
                *index += 1;
                if let Command::SubFileRef(sfr_cmd) = &cmd {
                    if let SubFileRef::ResolvedRef(resolved_ref) = &sfr_cmd.file {
                        let source_file_2 = resolved_ref.get(self.source_map);
                        let local_transform = Mat4::from_cols(
                            Vec4::new(sfr_cmd.row0.x, sfr_cmd.row1.x, sfr_cmd.row2.x, 0.0),
                            Vec4::new(sfr_cmd.row0.y, sfr_cmd.row1.y, sfr_cmd.row2.y, 0.0),
                            Vec4::new(sfr_cmd.row0.z, sfr_cmd.row1.z, sfr_cmd.row2.z, 0.0),
                            Vec4::new(sfr_cmd.pos.x, sfr_cmd.pos.y, sfr_cmd.pos.z, 1.0),
                        );
                        let draw_ctx = DrawContext {
                            transform: draw_ctx.transform * local_transform,
                            color: 16,
                        };
                        self.stack.push((source_file_2, 0, draw_ctx));
                        continue;
                    }
                } else if let Command::Comment(_) = &cmd {
                    // Skip comments
                    continue;
                }
                return Some((*draw_ctx, cmd));
            }
            self.stack.pop();
        }
        None
    }
}

impl<'a> Iterator for LocalCommandIterator<'a> {
    type Item = &'a Command;

    fn next(&mut self) -> Option<&'a Command> {
        while let Some(source_file) = self.stack.last() {
            let cmds = &source_file.cmds;
            if self.index < cmds.len() {
                let index = self.index;
                self.index += 1;
                return Some(&cmds[index]);
            }
            self.index = 0;
            self.stack.pop();
        }
        None
    }
}

impl ResolveQueue {
    fn new() -> ResolveQueue {
        return ResolveQueue {
            queue: vec![],
            pending_count: HashMap::new(),
        };
    }

    fn push(&mut self, filename: &str, referer_filename: &str, referer: SourceFileRef) {
        if let Some(num_pending) = self.pending_count.get_mut(referer_filename) {
            assert!(*num_pending > 0); // should not make it to the queue if already resolved
            *num_pending += 1;
        } else {
            self.pending_count.insert(referer_filename.to_string(), 1);
        }
        self.queue.push(QueuedFileRef {
            filename: filename.to_string(),
            referer,
        });
    }

    fn pop(&mut self, source_map: &SourceMap) -> Option<(QueuedFileRef, u32)> {
        match self.queue.pop() {
            Some(qfr) => {
                let num_pending = self
                    .pending_count
                    .get_mut(&qfr.referer.get(source_map).filename)
                    .unwrap();
                *num_pending -= 1;
                Some((qfr, *num_pending))
            }
            None => None,
        }
    }

    fn reset(&mut self) {
        self.queue.clear();
        self.pending_count.clear();
    }
}

fn load_and_parse_single_file(
    filename: &str,
    resolver: &dyn FileRefResolver,
) -> Result<SourceFile, Error> {
    //match resolver.resolve(filename) {}
    let raw_content = resolver.resolve(filename)?;
    let mut source_file = SourceFile {
        filename: filename.to_string(),
        raw_content,
        cmds: Vec::new(),
    };
    let cmds = parse_raw_with_filename(&source_file.filename[..], &source_file.raw_content[..])?;
    source_file.cmds = cmds;
    Ok(source_file)
}

/// Parse a single file and its sub-file references recursively.
///
/// Attempt to load the content of `filename` via the given `resolver`, and parse it.
/// Then recursiverly look for sub-file commands inside that root file, and try to resolve
/// the content of those sub-files and parse them too. All the loaded and parsed files end
/// up populating the given `source_map`, which can be pre-populated manually or from a
/// previous call with already loaded and parsed files.
/// ```rust
/// use weldr::{ FileRefResolver, parse, ResolveError, SourceMap };
///
/// struct MyCustomResolver;
///
/// impl FileRefResolver for MyCustomResolver {
///   fn resolve(&self, filename: &str) -> Result<Vec<u8>, ResolveError> {
///     Ok(vec![]) // replace with custom impl
///   }
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///   let resolver = MyCustomResolver{};
///   let mut source_map = SourceMap::new();
///   let root_file_ref = parse("root.ldr", &resolver, &mut source_map)?;
///   let root_file = root_file_ref.get(&source_map);
///   assert_eq!(root_file.filename, "root.ldr");
///   Ok(())
/// }
/// ```
pub fn parse(
    filename: &str,
    resolver: &dyn FileRefResolver,
    source_map: &mut SourceMap,
) -> Result<SourceFileRef, Error> {
    if let Some(existing_file) = source_map.find_filename(filename) {
        return Ok(existing_file);
    }
    debug!("Processing root file '{}'", filename);
    let root_file = load_and_parse_single_file(filename, resolver)?;
    trace!(
        "Post-loading resolving subfile refs of root file: {}",
        filename
    );
    let root_file_ref = source_map.insert(root_file);
    let mut queue = ResolveQueue::new();
    source_map.resolve_file_refs(root_file_ref, &mut queue);
    while let Some(queued_file) = queue.pop(source_map) {
        let num_pending_left = queued_file.1;
        let filename = &queued_file.0.filename;
        debug!("Processing sub-file: '{}'", filename);
        match source_map.find_filename(filename) {
            Some(_) => trace!("Already parsed; reusing sub-file: {}", filename),
            None => {
                trace!("Not yet parsed; parsing sub-file: {}", filename);
                let source_file = load_and_parse_single_file(&filename[..], resolver)?;
                let source_file_ref = source_map.insert(source_file);
                trace!(
                    "Post-loading resolving subfile refs of sub-file: {}",
                    filename
                );
                source_map.resolve_file_refs(source_file_ref, &mut queue);
            }
        }
        // Re-resolve the source file that triggered this sub-file loading to update its subfile refs
        // if there is no more sub-file references enqueued.
        if num_pending_left == 0 {
            trace!(
                "Re-resolving referer file on last resolved ref: {}",
                queued_file.0.referer.get(source_map).filename
            );
            source_map.resolve_file_refs(queued_file.0.referer, &mut queue);
        }
    }
    Ok(root_file_ref)
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!CATEGORY language extension](https://www.ldraw.org/article/340.html#category).
#[derive(Debug, PartialEq)]
pub struct CategoryCmd {
    /// Category name.
    pub category: String,
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!KEYWORDS language extension](https://www.ldraw.org/article/340.html#keywords).
#[derive(Debug, PartialEq)]
pub struct KeywordsCmd {
    /// List of keywords.
    pub keywords: Vec<String>,
}

/// Finish for color definitions ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq)]
pub enum ColorFinish {
    Chrome,
    Pearlescent,
    Rubber,
    MatteMetallic,
    Metal,
    Material(MaterialFinish),
}

/// Finish for optional MATERIAL part of color definition
/// ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq)]
pub enum MaterialFinish {
    Glitter(GlitterMaterial),
    Speckle(SpeckleMaterial),
    Other(String),
}

/// Grain size variants for the optional MATERIAL part of color definition
/// ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq)]
pub enum GrainSize {
    Size(f32),
    MinMaxSize((f32, f32)),
}

/// Glitter material definition of a color definition
/// ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq)]
pub struct GlitterMaterial {
    /// Primary color value of the material.
    pub value: Color,
    /// Optional alpha (opacity) value.
    pub alpha: Option<u8>,
    /// Optional brightness value.
    pub luminance: Option<u8>,
    /// Fraction of the surface using the alternate color.
    pub surface_fraction: f32,
    /// Fraction of the volume using the alternate color.
    pub volume_fraction: f32,
    /// Size of glitter grains.
    pub size: GrainSize,
}

/// Speckle material definition of a color definition
/// ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq)]
pub struct SpeckleMaterial {
    /// Primary color value of the material.
    pub value: Color,
    /// Optional alpha (opacity) value.
    pub alpha: Option<u8>,
    /// Optional brightness value.
    pub luminance: Option<u8>,
    /// Fraction of the surface using the alternate color.
    pub surface_fraction: f32,
    /// Size of speckle grains.
    pub size: GrainSize,
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!COLOUR language extension](https://www.ldraw.org/article/299.html).
#[derive(Debug, PartialEq)]
pub struct ColourCmd {
    /// Name of the color.
    pub name: String,
    /// Color code uniquely identifying this color. Codes 16 and 24 are reserved.
    pub code: u32,
    /// Primary value of the color.
    pub value: Color,
    /// Contrasting edge value of the color.
    pub edge: Color,
    /// Optional alpha (opacity) value.
    pub alpha: Option<u8>,
    /// Optional ["brightness for colors that glow"](https://www.ldraw.org/article/299.html#luminance).
    pub luminance: Option<u8>,
    /// Finish/texture of the object for high-fidelity rendering.
    pub finish: Option<ColorFinish>,
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) comment.
#[derive(Debug, PartialEq)]
pub struct CommentCmd {
    /// Comment content, excluding the command identififer `0` and the optional comment marker `//`.
    pub text: String,
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
    pub cmds: Vec<Command>,
}

/// Collection of [`SourceFile`] accessible from their reference filename.
#[derive(Debug)]
pub struct SourceMap {
    /// Array of source files in the collection.
    source_files: Vec<SourceFile>,

    /// Map table of source files indices into [`SourceMap::source_files`] from
    /// their reference filename, as it appears in [`SubFileRefCmd`].
    filename_map: HashMap<String, usize>,
}

/// Reference to a single [`SourceFile`] instance in a given [`SourceMap`].
#[derive(Debug, Copy, Clone, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SourceFileRef {
    index: usize,
    // TODO: ref to SourceMap?
}

impl CommentCmd {
    pub fn new(text: &str) -> CommentCmd {
        CommentCmd {
            text: text.to_string(),
        }
    }
}

impl SourceFileRef {
    pub fn get<'a>(&'a self, source_map: &'a SourceMap) -> &'a SourceFile {
        source_map.get(*self)
    }
}

impl SourceMap {
    /// Construct a new empty source map.
    pub fn new() -> SourceMap {
        SourceMap {
            source_files: vec![],
            filename_map: HashMap::new(),
        }
    }

    /// Get a reference to the source file corresponding to a [`SourceFileRef`].
    fn get(&self, source_file_ref: SourceFileRef) -> &SourceFile {
        &self.source_files[source_file_ref.index]
    }

    /// Get a mutable reference to the source file corresponding to a [`SourceFileRef`].
    fn get_mut(&mut self, source_file_ref: SourceFileRef) -> &mut SourceFile {
        &mut self.source_files[source_file_ref.index]
    }

    /// Find a source file by its reference filename.
    fn find_filename(&self, filename: &str) -> Option<SourceFileRef> {
        match self.filename_map.get(filename) {
            Some(&index) => Some(SourceFileRef { index }),
            None => None,
        }
    }

    /// Insert a new source file into the collection.
    fn insert(&mut self, source_file: SourceFile) -> SourceFileRef {
        if let Some(&index) = self.filename_map.get(&source_file.filename) {
            return SourceFileRef { index };
        } else {
            let index = self.source_files.len();
            self.filename_map
                .insert(source_file.filename.clone(), index);
            self.source_files.push(source_file);
            return SourceFileRef { index };
        }
    }

    /// Attempt to resolve the sub-file references of a given source file, and insert unresolved
    /// references into the given [`ResolveQueue`].
    fn resolve_file_refs(&mut self, source_file_ref: SourceFileRef, queue: &mut ResolveQueue) {
        // Steal filename map to decouple its lifetime from the one of the source files vector,
        // and allow lookup while mutably iterating over the source files and their commands
        let mut filename_map = HashMap::new();
        std::mem::swap(&mut filename_map, &mut self.filename_map);

        // Iterate over the commands of the current file and try to enqueue all its unresolved
        // sub-file reference commands for deferred resolution.
        let source_file = self.get_mut(source_file_ref);
        let referer_filename = source_file.filename.clone();
        let mut subfile_set = HashSet::new();
        for cmd in &mut source_file.cmds {
            if let Command::SubFileRef(sfr_cmd) = cmd {
                // All subfile refs come out of parse_raw() as unresolved by definition,
                // since parse_raw() doesn't have access to the source map nor the resolver.
                // See if we can resolve some of them now.
                if let SubFileRef::UnresolvedRef(subfilename) = &sfr_cmd.file {
                    let subfilename = subfilename.clone();
                    // Check the source map
                    if let Some(existing_source_file) = filename_map
                        .get(&subfilename)
                        .map(|&index| SourceFileRef { index })
                    {
                        // If already parsed, reuse
                        trace!(
                            "Updating resolved subfile ref in {} -> {}",
                            referer_filename,
                            subfilename
                        );
                        sfr_cmd.file = SubFileRef::ResolvedRef(existing_source_file);
                    } else if subfile_set.contains(&subfilename) {
                        trace!(
                            "Ignoring already-queued unresolved subfile ref in {} -> {}",
                            referer_filename,
                            subfilename
                        );
                    } else {
                        // If not, push to queue for later parsing, but only once
                        trace!(
                            "Queuing unresolved subfile ref in {} -> {}",
                            referer_filename,
                            subfilename
                        );
                        queue.push(&subfilename[..], &referer_filename[..], source_file_ref);
                        subfile_set.insert(subfilename);
                    }
                }
            }
        }

        // Restore the filename map
        std::mem::swap(&mut filename_map, &mut self.filename_map);
    }
}

/// Reference to a sub-file from inside another file.
#[derive(Debug, PartialEq)]
pub enum SubFileRef {
    /// Resolved reference pointing to the given loaded/parsed sub-file.
    ResolvedRef(SourceFileRef),
    /// Unresolved reference containing the raw reference filename.
    UnresolvedRef(String),
}

/// [Line Type 1](https://www.ldraw.org/article/218.html#lt1) LDraw command:
/// Reference a sub-file from the current file.
#[derive(Debug, PartialEq)]
pub struct SubFileRefCmd {
    /// Color code of the part.
    pub color: u32,
    /// Position.
    pub pos: Vec3,
    /// First row of rotation+scaling matrix part.
    pub row0: Vec3,
    /// Second row of rotation+scaling matrix part.
    pub row1: Vec3,
    /// Third row of rotation+scaling matrix part.
    pub row2: Vec3,
    /// Referenced sub-file.
    pub file: SubFileRef,
}

/// [Line Type 2](https://www.ldraw.org/article/218.html#lt2) LDraw command:
/// Draw a segment between 2 vertices.
#[derive(Debug, PartialEq)]
pub struct LineCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the segment.
    pub vertices: [Vec3; 2],
}

/// [Line Type 3](https://www.ldraw.org/article/218.html#lt3) LDraw command:
/// Draw a triangle between 3 vertices.
#[derive(Debug, PartialEq)]
pub struct TriangleCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the triangle.
    pub vertices: [Vec3; 3],
}

/// [Line Type 4](https://www.ldraw.org/article/218.html#lt4) LDraw command:
/// Draw a quad between 4 vertices.
#[derive(Debug, PartialEq)]
pub struct QuadCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the quad. In theory they are guaranteed to be coplanar according to the LDraw
    /// specification, although no attempt is made to validate this property.
    pub vertices: [Vec3; 4],
}

/// [Line Type 5](https://www.ldraw.org/article/218.html#lt5) LDraw command:
/// Draw an optional segment between two vertices, aided by 2 control points.
#[derive(Debug, PartialEq)]
pub struct OptLineCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the segment.
    pub vertices: [Vec3; 2],
    /// Control points of the segment.
    pub control_points: [Vec3; 2],
}

/// Types of commands contained in a LDraw file.
#[derive(Debug, PartialEq)]
pub enum Command {
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [!CATEGORY language extension](https://www.ldraw.org/article/340.html#category).
    Category(CategoryCmd),
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [!KEYWORDS language extension](https://www.ldraw.org/article/340.html#keywords).
    Keywords(KeywordsCmd),
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [!COLOUR language extension](https://www.ldraw.org/article/299.html).
    Colour(ColourCmd),
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
    OptLine(OptLineCmd),
}

/// Resolver trait for sub-file references ([Line Type 1](https://www.ldraw.org/article/218.html#lt1) LDraw command).
///
/// An implementation of this trait must be passed to [`parse()`] to allow resolving sub-file references recursively,
/// and parsing all dependent sub-files of the top-level file provided.
///
/// When loading parts and primitives from the official LDraw catalog, implementations are free to decide how to retrieve
/// the file content, but must ensure that all canonical paths are in scope, as sub-file references can be relative to
/// any of those:
/// - `/p/`       - Parts primitives
/// - `/p/48/`    - High-resolution primitives
/// - `/parts/`   - Main catalog of parts
/// - `/parts/s/` - Catalog of sub-parts commonly used
pub trait FileRefResolver {
    /// Resolve the given file reference `filename`, given as it appears in a sub-file reference, and return
    /// the content of the file as a UTF-8 encoded buffer of bytes, without BOM. Line ending can be indifferently
    /// Unix style `\n` or Windows style `\r\n`.
    ///
    /// See [`parse()`] for usage.
    fn resolve(&self, filename: &str) -> Result<Vec<u8>, ResolveError>;
}

#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;

    use super::*;

    #[test]
    fn test_color_id() {
        assert_eq!(color_id(b""), Err(nom_error(&b""[..], ErrorKind::Digit)));
        assert_eq!(color_id(b"1"), Ok((&b""[..], 1)));
        assert_eq!(color_id(b"16 "), Ok((&b" "[..], 16)));
    }

    #[test]
    fn test_from_hex() {
        assert_eq!(from_hex(b"0"), Ok(0));
        assert_eq!(from_hex(b"1"), Ok(1));
        assert_eq!(from_hex(b"a"), Ok(10));
        assert_eq!(from_hex(b"F"), Ok(15));
        assert_eq!(from_hex(b"G"), Err(ErrorKind::AlphaNumeric));
        assert_eq!(from_hex(b"10"), Ok(16));
        assert_eq!(from_hex(b"FF"), Ok(255));
        assert_eq!(from_hex(b"1G"), Err(ErrorKind::AlphaNumeric));
        assert_eq!(from_hex(b"100"), Err(ErrorKind::AlphaNumeric));
        assert_eq!(from_hex(b"\xFF"), Err(ErrorKind::AlphaNumeric));
    }

    #[test]
    fn test_hex_color() {
        assert_eq!(hex_color(b""), Err(nom_error(&b""[..], ErrorKind::Tag)));
        assert_eq!(
            hex_color(b"#"),
            Err(nom_error(&b""[..], ErrorKind::TakeWhileMN))
        );
        assert_eq!(
            hex_color(b"#1"),
            Err(nom_error(&b"1"[..], ErrorKind::TakeWhileMN))
        );
        assert_eq!(
            hex_color(b"#12345Z"),
            Err(nom_error(&b"5Z"[..], ErrorKind::TakeWhileMN))
        );
        assert_eq!(
            hex_color(b"#123456"),
            Ok((&b""[..], Color::new(0x12, 0x34, 0x56)))
        );
        assert_eq!(
            hex_color(b"#ABCDEF"),
            Ok((&b""[..], Color::new(0xAB, 0xCD, 0xEF)))
        );
        assert_eq!(
            hex_color(b"#8E5cAf"),
            Ok((&b""[..], Color::new(0x8E, 0x5C, 0xAF)))
        );
        assert_eq!(
            hex_color(b"#123456e"),
            Ok((&b"e"[..], Color::new(0x12, 0x34, 0x56)))
        );
    }

    #[test]
    fn test_colour_alpha() {
        assert_eq!(colour_alpha(b""), Ok((&b""[..], None)));
        assert_eq!(colour_alpha(b" ALPHA 0"), Ok((&b""[..], Some(0))));
        assert_eq!(colour_alpha(b" ALPHA 1"), Ok((&b""[..], Some(1))));
        assert_eq!(colour_alpha(b" ALPHA 128"), Ok((&b""[..], Some(128))));
        assert_eq!(colour_alpha(b" ALPHA 255"), Ok((&b""[..], Some(255))));
        assert_eq!(colour_alpha(b" ALPHA 34 "), Ok((&b" "[..], Some(34))));
        // TODO - Should fail on partial match, but succeeds because of opt!()
        assert_eq!(colour_alpha(b" ALPHA"), Ok((&b" ALPHA"[..], None))); // Err(Err::Incomplete(Needed::Size(1)))
        assert_eq!(colour_alpha(b" ALPHA 256"), Ok((&b" ALPHA 256"[..], None)));
        // Err(Err::Incomplete(Needed::Size(1)))
    }

    #[test]
    fn test_colour_luminance() {
        assert_eq!(colour_luminance(b""), Ok((&b""[..], None)));
        assert_eq!(colour_luminance(b" LUMINANCE 0"), Ok((&b""[..], Some(0))));
        assert_eq!(colour_luminance(b" LUMINANCE 1"), Ok((&b""[..], Some(1))));
        assert_eq!(
            colour_luminance(b" LUMINANCE 128"),
            Ok((&b""[..], Some(128)))
        );
        assert_eq!(
            colour_luminance(b" LUMINANCE 255"),
            Ok((&b""[..], Some(255)))
        );
        assert_eq!(
            colour_luminance(b" LUMINANCE 34 "),
            Ok((&b" "[..], Some(34)))
        );
        // TODO - Should fail on partial match, but succeeds because of opt!()
        assert_eq!(
            colour_luminance(b" LUMINANCE"),
            Ok((&b" LUMINANCE"[..], None))
        ); // Err(Err::Incomplete(Needed::Size(1)))
        assert_eq!(
            colour_luminance(b" LUMINANCE 256"),
            Ok((&b" LUMINANCE 256"[..], None))
        ); // Err(Err::Incomplete(Needed::Size(1)))
    }

    #[test]
    fn test_material_grain_size() {
        assert_eq!(
            material_grain_size(b""),
            Err(nom_error(&b""[..], ErrorKind::Tag))
        );
        assert_eq!(
            material_grain_size(b"SIZE"),
            Err(nom_error(&b"SIZE"[..], ErrorKind::Tag))
        );
        assert_eq!(
            material_grain_size(b"SIZE 1"),
            Ok((&b""[..], GrainSize::Size(1.0)))
        );
        assert_eq!(
            material_grain_size(b"SIZE 0.02"),
            Ok((&b""[..], GrainSize::Size(0.02)))
        );
        assert_eq!(
            material_grain_size(b"MINSIZE"),
            Err(nom_error(&b""[..], ErrorKind::Space))
        );
        assert_eq!(
            material_grain_size(b"MINSIZE 0.02"),
            Err(nom_error(&b""[..], ErrorKind::Space))
        );
        assert_eq!(
            material_grain_size(b"MINSIZE 0.02 MAXSIZE 0.04"),
            Ok((&b""[..], GrainSize::MinMaxSize((0.02, 0.04))))
        );
    }

    #[test]
    fn test_glitter_material() {
        assert_eq!(
            glitter_material(b""),
            Err(nom_error(&b""[..], ErrorKind::Tag))
        );
        assert_eq!(
            glitter_material(b"GLITTER"),
            Err(nom_error(&b""[..], ErrorKind::Space))
        );
        assert_eq!(
            glitter_material(b"GLITTER VALUE #123456 FRACTION 1.0 VFRACTION 0.3 SIZE 1"),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Glitter(GlitterMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: None,
                    luminance: None,
                    surface_fraction: 1.0,
                    volume_fraction: 0.3,
                    size: GrainSize::Size(1.0)
                }))
            ))
        );
        assert_eq!(
            glitter_material(b"GLITTER VALUE #123456 ALPHA 128 FRACTION 1.0 VFRACTION 0.3 SIZE 1"),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Glitter(GlitterMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: Some(128),
                    luminance: None,
                    surface_fraction: 1.0,
                    volume_fraction: 0.3,
                    size: GrainSize::Size(1.0)
                }))
            ))
        );
        assert_eq!(
            glitter_material(
                b"GLITTER VALUE #123456 LUMINANCE 32 FRACTION 1.0 VFRACTION 0.3 SIZE 1"
            ),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Glitter(GlitterMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: None,
                    luminance: Some(32),
                    surface_fraction: 1.0,
                    volume_fraction: 0.3,
                    size: GrainSize::Size(1.0)
                }))
            ))
        );
        assert_eq!(
            glitter_material(
                b"GLITTER VALUE #123456 FRACTION 1.0 VFRACTION 0.3 MINSIZE 0.02 MAXSIZE 0.04"
            ),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Glitter(GlitterMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: None,
                    luminance: None,
                    surface_fraction: 1.0,
                    volume_fraction: 0.3,
                    size: GrainSize::MinMaxSize((0.02, 0.04))
                }))
            ))
        );
    }

    #[test]
    fn test_speckle_material() {
        assert_eq!(
            speckle_material(b""),
            Err(nom_error(&b""[..], ErrorKind::Tag))
        );
        assert_eq!(
            speckle_material(b"SPECKLE"),
            Err(nom_error(&b""[..], ErrorKind::Space))
        );
        assert_eq!(
            speckle_material(b"SPECKLE VALUE #123456 FRACTION 1.0 SIZE 1"),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Speckle(SpeckleMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: None,
                    luminance: None,
                    surface_fraction: 1.0,
                    size: GrainSize::Size(1.0)
                }))
            ))
        );
        assert_eq!(
            speckle_material(b"SPECKLE VALUE #123456 ALPHA 128 FRACTION 1.0 SIZE 1"),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Speckle(SpeckleMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: Some(128),
                    luminance: None,
                    surface_fraction: 1.0,
                    size: GrainSize::Size(1.0)
                }))
            ))
        );
        assert_eq!(
            speckle_material(b"SPECKLE VALUE #123456 LUMINANCE 32 FRACTION 1.0 SIZE 1"),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Speckle(SpeckleMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: None,
                    luminance: Some(32),
                    surface_fraction: 1.0,
                    size: GrainSize::Size(1.0)
                }))
            ))
        );
        assert_eq!(
            speckle_material(b"SPECKLE VALUE #123456 FRACTION 1.0 MINSIZE 0.02 MAXSIZE 0.04"),
            Ok((
                &b""[..],
                ColorFinish::Material(MaterialFinish::Speckle(SpeckleMaterial {
                    value: Color::new(0x12, 0x34, 0x56),
                    alpha: None,
                    luminance: None,
                    surface_fraction: 1.0,
                    size: GrainSize::MinMaxSize((0.02, 0.04))
                }))
            ))
        );
    }

    #[test]
    fn test_color_finish() {
        assert_eq!(color_finish(b""), Ok((&b""[..], None)));
        assert_eq!(color_finish(b"CHROME"), Ok((&b"CHROME"[..], None)));
        assert_eq!(
            color_finish(b" CHROME"),
            Ok((&b""[..], Some(ColorFinish::Chrome)))
        );
        assert_eq!(
            color_finish(b" PEARLESCENT"),
            Ok((&b""[..], Some(ColorFinish::Pearlescent)))
        );
        assert_eq!(
            color_finish(b" RUBBER"),
            Ok((&b""[..], Some(ColorFinish::Rubber)))
        );
        assert_eq!(
            color_finish(b" MATTE_METALLIC"),
            Ok((&b""[..], Some(ColorFinish::MatteMetallic)))
        );
        assert_eq!(
            color_finish(b" METAL"),
            Ok((&b""[..], Some(ColorFinish::Metal)))
        );
        // TODO - Should probably ensure <SPACE> or <EOF> after keyword, not *anything*
        assert_eq!(
            color_finish(b" CHROMEas"),
            Ok((&b"as"[..], Some(ColorFinish::Chrome)))
        );
        assert_eq!(
            color_finish(b" MATERIAL custom values"),
            Ok((
                &b""[..],
                Some(ColorFinish::Material(MaterialFinish::Other(
                    "custom values".to_string()
                )))
            ))
        );
    }

    #[test]
    fn test_digit1_as_u8() {
        assert_eq!(
            digit1_as_u8(b""),
            Err(nom_error(&b""[..], ErrorKind::Digit))
        );
        assert_eq!(digit1_as_u8(b"0"), Ok((&b""[..], 0u8)));
        assert_eq!(digit1_as_u8(b"1"), Ok((&b""[..], 1u8)));
        assert_eq!(digit1_as_u8(b"255"), Ok((&b""[..], 255u8)));
        assert_eq!(
            digit1_as_u8(b"256"),
            Err(nom_error(&b"256"[..], ErrorKind::MapRes))
        );
        assert_eq!(digit1_as_u8(b"32 "), Ok((&b" "[..], 32u8)));
    }

    #[test]
    fn test_meta_colour() {
        assert_eq!(meta_colour(b""), Err(nom_error(&b""[..], ErrorKind::Tag)));
        assert_eq!(
            meta_colour(b"!COLOUR test_col CODE 20 VALUE #123456"),
            Err(nom_error(&b""[..], ErrorKind::Space))
        );
        assert_eq!(
            meta_colour(b"!COLOUR test_col CODE 20 VALUE #123456 EDGE #abcdef"),
            Ok((
                &b""[..],
                Command::Colour(ColourCmd {
                    name: "test_col".to_string(),
                    code: 20,
                    value: Color::new(0x12, 0x34, 0x56),
                    edge: Color::new(0xAB, 0xCD, 0xEF),
                    alpha: None,
                    luminance: None,
                    finish: None
                })
            ))
        );
        assert_eq!(
            meta_colour(b"!COLOUR test_col CODE 20 VALUE #123456 EDGE #abcdef ALPHA 128"),
            Ok((
                &b""[..],
                Command::Colour(ColourCmd {
                    name: "test_col".to_string(),
                    code: 20,
                    value: Color::new(0x12, 0x34, 0x56),
                    edge: Color::new(0xAB, 0xCD, 0xEF),
                    alpha: Some(128),
                    luminance: None,
                    finish: None
                })
            ))
        );
        assert_eq!(
            meta_colour(b"!COLOUR test_col CODE 20 VALUE #123456 EDGE #abcdef LUMINANCE 32"),
            Ok((
                &b""[..],
                Command::Colour(ColourCmd {
                    name: "test_col".to_string(),
                    code: 20,
                    value: Color::new(0x12, 0x34, 0x56),
                    edge: Color::new(0xAB, 0xCD, 0xEF),
                    alpha: None,
                    luminance: Some(32),
                    finish: None
                })
            ))
        );
        assert_eq!(
            meta_colour(
                b"!COLOUR test_col CODE 20 VALUE #123456 EDGE #abcdef ALPHA 64 LUMINANCE 32"
            ),
            Ok((
                &b""[..],
                Command::Colour(ColourCmd {
                    name: "test_col".to_string(),
                    code: 20,
                    value: Color::new(0x12, 0x34, 0x56),
                    edge: Color::new(0xAB, 0xCD, 0xEF),
                    alpha: Some(64),
                    luminance: Some(32),
                    finish: None
                })
            ))
        );
        assert_eq!(
            meta_colour(b"!COLOUR test_col CODE 20 VALUE #123456 EDGE #abcdef CHROME"),
            Ok((
                &b""[..],
                Command::Colour(ColourCmd {
                    name: "test_col".to_string(),
                    code: 20,
                    value: Color::new(0x12, 0x34, 0x56),
                    edge: Color::new(0xAB, 0xCD, 0xEF),
                    alpha: None,
                    luminance: None,
                    finish: Some(ColorFinish::Chrome)
                })
            ))
        );
        assert_eq!(
            meta_colour(b"!COLOUR test_col CODE 20 VALUE #123456 EDGE #abcdef ALPHA 128 RUBBER"),
            Ok((
                &b""[..],
                Command::Colour(ColourCmd {
                    name: "test_col".to_string(),
                    code: 20,
                    value: Color::new(0x12, 0x34, 0x56),
                    edge: Color::new(0xAB, 0xCD, 0xEF),
                    alpha: Some(128),
                    luminance: None,
                    finish: Some(ColorFinish::Rubber)
                })
            ))
        );
    }

    #[test]
    fn test_vec3() {
        assert_eq!(
            read_vec3(b"0 0 0"),
            Ok((&b""[..], Vec3::new(0.0, 0.0, 0.0)))
        );
        assert_eq!(
            read_vec3(b"0 0 0 1"),
            Ok((&b" 1"[..], Vec3::new(0.0, 0.0, 0.0)))
        );
        assert_eq!(
            read_vec3(b"2 5 -7"),
            Ok((&b""[..], Vec3::new(2.0, 5.0, -7.0)))
        );
        assert_eq!(
            read_vec3(b"2.3 5 -7.4"),
            Ok((&b""[..], Vec3::new(2.3, 5.0, -7.4)))
        );
    }

    #[test]
    fn test_read_cmd_id_str() {
        assert_eq!(read_cmd_id_str(b"0"), Ok((&b""[..], &b"0"[..])));
        assert_eq!(read_cmd_id_str(b"0 "), Ok((&b""[..], &b"0"[..])));
        assert_eq!(read_cmd_id_str(b"0   "), Ok((&b""[..], &b"0"[..])));
        assert_eq!(read_cmd_id_str(b"0   e"), Ok((&b"e"[..], &b"0"[..])));
        assert_eq!(
            read_cmd_id_str(b"4547    ssd"),
            Ok((&b"ssd"[..], &b"4547"[..]))
        );
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
        assert_eq!(
            take_not_cr_or_lf(b"\r\n\r\n\r\n"),
            Ok((&b"\r\n\r\n\r\n"[..], &b""[..]))
        );
        assert_eq!(take_not_cr_or_lf(b" a \n"), Ok((&b"\n"[..], &b" a "[..])));
        assert_eq!(take_not_cr_or_lf(b"test"), Ok((&b""[..], &b"test"[..])));
    }

    #[test]
    fn test_single_comma() {
        assert_eq!(single_comma(b""), Err(nom_error(&b""[..], ErrorKind::Tag)));
        assert_eq!(single_comma(b","), Ok((&b""[..], &b","[..])));
        assert_eq!(single_comma(b",s"), Ok((&b"s"[..], &b","[..])));
        assert_eq!(
            single_comma(b"w,s"),
            Err(nom_error(&b"w,s"[..], ErrorKind::Tag))
        );
    }

    #[test]
    fn test_keywords_list() {
        assert_eq!(
            keywords_list(b""),
            Err(nom_error(&b""[..], ErrorKind::TakeWhile1))
        );
        assert_eq!(keywords_list(b"a"), Ok((&b""[..], vec!["a"])));
        assert_eq!(keywords_list(b"a,b,c"), Ok((&b""[..], vec!["a", "b", "c"])));
    }

    #[test]
    fn test_filename_char() {
        assert_eq!(
            filename_char(b""),
            Err(nom_error(&b""[..], ErrorKind::AlphaNumeric))
        );
        assert_eq!(filename_char(b"a"), Ok((&b""[..], &b"a"[..])));
        assert_eq!(filename_char(b"a-"), Ok((&b""[..], &b"a-"[..])));
        assert_eq!(
            filename_char(b"a/sad.bak\\ww.dat"),
            Ok((&b""[..], &b"a/sad.bak\\ww.dat"[..]))
        );
    }

    #[test]
    fn test_filename() {
        assert_eq!(filename(b"asd\\kw/l.ldr"), Ok((&b""[..], "asd\\kw/l.ldr")));
        assert_eq!(filename(b"asdkwl.ldr"), Ok((&b""[..], "asdkwl.ldr")));
        assert_eq!(
            filename(b"asd\\kw/l.ldr\n"),
            Ok((&b"\n"[..], "asd\\kw/l.ldr"))
        );
        assert_eq!(filename(b"asdkwl.ldr\n"), Ok((&b"\n"[..], "asdkwl.ldr")));
        assert_eq!(
            filename(b"asd\\kw/l.ldr\r\n"),
            Ok((&b"\r\n"[..], "asd\\kw/l.ldr"))
        );
        assert_eq!(
            filename(b"asdkwl.ldr\r\n"),
            Ok((&b"\r\n"[..], "asdkwl.ldr"))
        );
    }

    #[test]
    fn test_category_cmd() {
        let res = Command::Category(CategoryCmd {
            category: "Figure Accessory".to_string(),
        });
        assert_eq!(category(b"!CATEGORY Figure Accessory"), Ok((&b""[..], res)));
    }

    #[test]
    fn test_keywords_cmd() {
        let res = Command::Keywords(KeywordsCmd {
            keywords: vec![
                "western".to_string(),
                "wild west".to_string(),
                "spaghetti western".to_string(),
                "horse opera".to_string(),
                "cowboy".to_string(),
            ],
        });
        assert_eq!(
            keywords(b"!KEYWORDS western, wild west, spaghetti western, horse opera, cowboy"),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_comment_cmd() {
        let comment = b"test of comment, with \"weird\" characters";
        let res = Command::Comment(CommentCmd::new(std::str::from_utf8(comment).unwrap()));
        assert_eq!(meta_cmd(comment), Ok((&b""[..], res)));
        // Match empty comment too (e.g. "0" line without anything else, or "0   " with only spaces)
        assert_eq!(
            meta_cmd(b""),
            Ok((&b""[..], Command::Comment(CommentCmd::new(""))))
        );
    }

    #[test]
    fn test_file_ref_cmd() {
        let res = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: SubFileRef::UnresolvedRef("aaaaaaddd".to_string()),
        });
        assert_eq!(
            file_ref_cmd(b"16 0 0 0 1 0 0 0 1 0 0 0 1 aaaaaaddd"),
            Ok((&b""[..], res))
        );
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
        assert_eq!(
            space_or_eol0(b"\n\r\r\r\n"),
            Ok((&b""[..], &b"\n\r\r\r\n"[..]))
        );
        assert_eq!(space_or_eol0(b"  \n"), Ok((&b""[..], &b"  \n"[..])));
        assert_eq!(space_or_eol0(b"  \n   "), Ok((&b""[..], &b"  \n   "[..])));
        assert_eq!(
            space_or_eol0(b"  \n   \r\n"),
            Ok((&b""[..], &b"  \n   \r\n"[..]))
        );
        assert_eq!(
            space_or_eol0(b"  \n   \r\n "),
            Ok((&b""[..], &b"  \n   \r\n "[..]))
        );
        assert_eq!(space_or_eol0(b"  \nsa"), Ok((&b"sa"[..], &b"  \n"[..])));
        assert_eq!(
            space_or_eol0(b"  \n  \r\nsa"),
            Ok((&b"sa"[..], &b"  \n  \r\n"[..]))
        );
    }

    #[test]
    fn test_empty_line() {
        assert_eq!(empty_line(b""), Ok((&b""[..], &b""[..])));
        assert_eq!(empty_line(b" "), Ok((&b""[..], &b" "[..])));
        assert_eq!(empty_line(b"   "), Ok((&b""[..], &b"   "[..])));
        assert_eq!(
            empty_line(b"  a"),
            Err(nom_error(&b"a"[..], ErrorKind::CrLf))
        );
        assert_eq!(
            empty_line(b"a  "),
            Err(nom_error(&b"a  "[..], ErrorKind::CrLf))
        );
    }

    #[test]
    fn test_read_cmd() {
        let res = Command::Comment(CommentCmd::new("this doesn't matter"));
        assert_eq!(read_line(b"0 this doesn't matter"), Ok((&b""[..], res)));
    }

    #[test]
    fn test_read_line_cmd() {
        let res = Command::Line(LineCmd {
            color: 16,
            vertices: [Vec3::new(1.0, 1.0, 0.0), Vec3::new(0.9239, 1.0, 0.3827)],
        });
        assert_eq!(
            read_line(b"2 16 1 1 0 0.9239 1 0.3827"),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_read_tri_cmd() {
        let res = Command::Triangle(TriangleCmd {
            color: 16,
            vertices: [
                Vec3::new(1.0, 1.0, 0.0),
                Vec3::new(0.9239, 1.0, 0.3827),
                Vec3::new(0.9239, 0.0, 0.3827),
            ],
        });
        assert_eq!(
            read_line(b"3 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827"),
            Ok((&b""[..], res))
        );
        let res = Command::Triangle(TriangleCmd {
            color: 16,
            vertices: [
                Vec3::new(1.0, 1.0, 0.0),
                Vec3::new(0.9239, 1.0, 0.3827),
                Vec3::new(0.9239, 0.0, 0.3827),
            ],
        });
        assert_eq!(
            // Note: extra spaces at end
            read_line(b"3 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827  "),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_read_quad_cmd() {
        let res = Command::Quad(QuadCmd {
            color: 16,
            vertices: [
                Vec3::new(1.0, 1.0, 0.0),
                Vec3::new(0.9239, 1.0, 0.3827),
                Vec3::new(0.9239, 0.0, 0.3827),
                Vec3::new(1.0, 0.0, 0.0),
            ],
        });
        assert_eq!(
            read_line(b"4 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0"),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_read_opt_line_cmd() {
        let res = Command::OptLine(OptLineCmd {
            color: 16,
            vertices: [Vec3::new(1.0, 1.0, 0.0), Vec3::new(0.9239, 1.0, 0.3827)],
            control_points: [Vec3::new(0.9239, 0.0, 0.3827), Vec3::new(1.0, 0.0, 0.0)],
        });
        assert_eq!(
            read_line(b"5 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0"),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_read_line_subfileref() {
        let res = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string()),
        });
        assert_eq!(
            read_line(b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_parse_raw() {
        let cmd0 = Command::Comment(CommentCmd::new("this is a comment"));
        let cmd1 = Command::Line(LineCmd {
            color: 16,
            vertices: [Vec3::new(0.0, 0.0, 0.0), Vec3::new(1.0, 1.0, 1.0)],
        });
        assert_eq!(
            parse_raw(b"0 this is a comment\n2 16 0 0 0 1 1 1").unwrap(),
            vec![cmd0, cmd1]
        );

        let cmd0 = Command::Comment(CommentCmd::new("this doesn't matter"));
        let cmd1 = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string()),
        });
        assert_eq!(
            parse_raw(b"\n0 this doesn't matter\n\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd")
                .unwrap(),
            vec![cmd0, cmd1]
        );

        let cmd0 = Command::Comment(CommentCmd::new("this doesn't \"matter\""));
        let cmd1 = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string()),
        });
        assert_eq!(
            parse_raw(
                b"\r\n0 this doesn't \"matter\"\r\n\r\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd\n"
            )
            .unwrap(),
            vec![cmd0, cmd1]
        );

        let cmd0 = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string()),
        });
        let cmd1 = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: SubFileRef::UnresolvedRef("aa/aaaaddd".to_string()),
        });
        assert_eq!(
            parse_raw(
                b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"
            )
            .unwrap(),
            vec![cmd0, cmd1]
        );
    }

    #[test]
    fn test_source_file_iter() {
        let mut source_map = SourceMap::new();
        let source_file = SourceFile {
            filename: "tata".to_string(),
            raw_content: vec![],
            cmds: vec![Command::Triangle(TriangleCmd {
                color: 2,
                vertices: [
                    Vec3::new(0.0, 0.0, 0.0),
                    Vec3::new(1.0, 0.0, 0.0),
                    Vec3::new(0.0, 1.0, 0.0),
                ],
            })],
        };
        let source_file_ref = source_map.insert(source_file);
        let s = SourceFile {
            filename: "toto".to_string(),
            raw_content: vec![],
            cmds: vec![
                Command::Triangle(TriangleCmd {
                    color: 16,
                    vertices: [
                        Vec3::new(0.0, 0.0, 1.0),
                        Vec3::new(1.0, 0.0, 1.0),
                        Vec3::new(0.0, 1.0, 1.0),
                    ],
                }),
                Command::Comment(CommentCmd::new("my comment")),
                Command::SubFileRef(SubFileRefCmd {
                    color: 24,
                    pos: Vec3::new(0.0, 0.0, 0.0),
                    row0: Vec3::new(0.0, 0.0, 0.0),
                    row1: Vec3::new(0.0, 0.0, 0.0),
                    row2: Vec3::new(0.0, 0.0, 0.0),
                    file: SubFileRef::ResolvedRef(source_file_ref),
                }),
                Command::Quad(QuadCmd {
                    color: 1,
                    vertices: [
                        Vec3::new(0.0, 1.0, 0.0),
                        Vec3::new(0.0, 1.0, 1.0),
                        Vec3::new(1.0, 1.0, 1.0),
                        Vec3::new(1.0, 1.0, 0.0),
                    ],
                }),
            ],
        };
        let source_file_ref = source_map.insert(s);
        let source_file = source_file_ref.get(&source_map);
        for c in source_file.iter(&source_map) {
            trace!("cmd: {:?}", c);
        }

        let cmds: Vec<_> = source_file.iter(&source_map).map(|(_, cmd)| cmd).collect();
        assert_eq!(3, cmds.len());
        assert!(matches!(&cmds[0], Command::Triangle(_)));
        assert!(matches!(&cmds[1], Command::Triangle(_)));
        assert!(matches!(&cmds[2], Command::Quad(_)));
        if let Command::Triangle(tri_cmd) = &cmds[0] {
            assert_eq!(16, tri_cmd.color);
        }
        if let Command::Triangle(tri_cmd) = &cmds[1] {
            assert_eq!(2, tri_cmd.color);
        }
        if let Command::Quad(quad_cmd) = &cmds[2] {
            assert_eq!(1, quad_cmd.color);
        }

        let cmds: Vec<_> = source_file.local_iter(&source_map).collect();
        assert_eq!(4, cmds.len());
        assert!(matches!(&cmds[0], Command::Triangle(_)));
        assert!(matches!(&cmds[1], Command::Comment(_)));
        assert!(matches!(&cmds[2], Command::SubFileRef(_)));
        assert!(matches!(&cmds[3], Command::Quad(_)));
        if let Command::Triangle(tri_cmd) = &cmds[0] {
            assert_eq!(16, tri_cmd.color);
        }
        if let Command::Comment(comment_cmd) = &cmds[1] {
            assert_eq!("my comment", comment_cmd.text);
        }
        if let Command::SubFileRef(sfr_cmd) = &cmds[2] {
            assert_eq!(24, sfr_cmd.color);
        }
        if let Command::Quad(quad_cmd) = &cmds[3] {
            assert_eq!(1, quad_cmd.color);
        }
    }
}
