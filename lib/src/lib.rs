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

use base64::Engine;
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
use std::{collections::HashMap, str};

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
    i.split_at_position_complete(is_cr_or_lf)
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
    i.split_at_position_complete(is_space)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    (c as char).is_ascii_hexdigit()
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

fn meta_file(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, _) = tag(b"FILE")(i)?;
    let (i, _) = sp(i)?;
    let (i, file) = map_res(take_not_cr_or_lf, str::from_utf8)(i)?;

    Ok((
        i,
        Command::File(FileCmd {
            file: file.to_string(),
        }),
    ))
}

fn meta_data(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, _) = tag(b"!DATA")(i)?;
    let (i, _) = sp(i)?;
    let (i, file) = map_res(take_not_cr_or_lf, str::from_utf8)(i)?;

    Ok((
        i,
        Command::Data(DataCmd {
            file: file.to_string(),
        }),
    ))
}

fn meta_base_64_data(i: &[u8]) -> IResult<&[u8], Command> {
    // TODO: Validate base64 characters?
    let (i, _) = tag(b"!:")(i)?;
    let (i, _) = sp(i)?;
    let (i, data) = map_res(take_not_cr_or_lf, |b| {
        base64::engine::general_purpose::STANDARD_NO_PAD.decode(b)
    })(i)?;

    Ok((i, Command::Base64Data(Base64DataCmd { data })))
}

fn meta_nofile(i: &[u8]) -> IResult<&[u8], Command> {
    let (i, _) = tag(b"NOFILE")(i)?;
    Ok((i, Command::NoFile))
}

fn meta_cmd(i: &[u8]) -> IResult<&[u8], Command> {
    alt((
        complete(category),
        complete(keywords),
        complete(meta_colour),
        complete(meta_file),
        complete(meta_nofile),
        complete(meta_data),
        complete(meta_base_64_data),
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
    // Assume leading and trailing whitespace isn't part of the filename.
    map(map_res(take_not_cr_or_lf, str::from_utf8), |s| s.trim())(i)
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
            file: file.into(),
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
            color,
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
            color,
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
            color,
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
/// let cmd0 = Command::Comment(CommentCmd::new("this is a comment"));
/// let cmd1 = Command::Line(LineCmd{
///   color: 16,
///   vertices: [
///     Vec3{ x: 0.0, y: 0.0, z: 0.0 },
///     Vec3{ x: 1.0, y: 1.0, z: 1.0 }
///   ]
/// });
/// assert_eq!(parse_raw(b"0 this is a comment\n2 16 0 0 0 1 1 1").unwrap(), vec![cmd0, cmd1]);
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

struct FileRef {
    /// Filename of unresolved source file.
    filename: String,
    /// Referer source file which requested the resolution.
    referer_filename: String,
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
            stack: vec![(self, 0, draw_ctx)],
            source_map,
        }
    }

    /// Return an iterator over all commands local to this source file, including sub-file references
    /// and comments. Unlike [`SourceFile::iter()`], this doesn't step into those sub-file references
    /// but remains in the local source file.
    pub fn local_iter<'a>(&'a self, source_map: &'a SourceMap) -> LocalCommandIterator<'a> {
        LocalCommandIterator {
            stack: vec![self],
            index: 0,
            source_map,
        }
    }
}

impl<'a> Iterator for CommandIterator<'a> {
    type Item = (DrawContext, &'a Command);

    fn next(&mut self) -> Option<(DrawContext, &'a Command)> {
        while let Some((file, index, draw_ctx)) = self.stack.last_mut() {
            let cmds = &file.cmds;
            if *index < cmds.len() {
                let cmd = &cmds[*index];
                *index += 1;
                if let Command::SubFileRef(sfr_cmd) = &cmd {
                    if let Some(source_file) = self.source_map.get(&sfr_cmd.file) {
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
                        self.stack.push((source_file, 0, draw_ctx));
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

fn load_and_parse_single_file(
    filename: &str,
    resolver: &dyn FileRefResolver,
) -> Result<SourceFile, Error> {
    // We should never have "unparsed" source files.
    // Caching only is always keyed by filenames anyway.
    let raw_content = resolver.resolve(filename)?;
    let cmds = parse_raw_with_filename(filename, &raw_content)?;
    Ok(SourceFile {
        filename: filename.to_string(),
        cmds,
    })
}

/// Parse a single file and its sub-file references recursively.
///
/// Attempt to load the content of `filename` via the given `resolver`, and parse it.
/// Then recursively look for sub-file commands inside that root file, and try to resolve
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
///   let main_model_name = parse("root.ldr", &resolver, &mut source_map)?;
///   let root_file = source_map.get(&main_model_name).unwrap();
///   assert_eq!(root_file.filename, "root.ldr");
///   Ok(())
/// }
/// ```
pub fn parse<R: FileRefResolver>(
    filename: &str,
    resolver: &R,
    source_map: &mut SourceMap,
) -> Result<String, Error> {
    if source_map.get(filename).is_some() {
        return Ok(filename.into());
    }

    // Use a stack to avoid function recursion in load_file.
    let mut stack: Vec<FileRef> = Vec::new();

    debug!("Processing root file '{}'", filename);
    let actual_root = load_file(filename, resolver, source_map, &mut stack)?;

    // Recursively load files referenced by the root file.
    while let Some(file) = stack.pop() {
        let filename = &file.filename;
        debug!("Processing sub-file: '{}'", filename);
        match source_map.get(filename) {
            Some(_) => trace!("Already parsed; reusing sub-file: {}", filename),
            None => {
                trace!("Not yet parsed; parsing sub-file: {}", filename);
                load_file(filename, resolver, source_map, &mut stack)?;
            }
        }
    }

    Ok(actual_root)
}

fn load_file<R: FileRefResolver>(
    filename: &str,
    resolver: &R,
    source_map: &mut SourceMap,
    stack: &mut Vec<FileRef>,
) -> Result<String, Error> {
    let source_file = load_and_parse_single_file(filename, resolver)?;
    source_map.queue_subfiles(&source_file, stack);
    Ok(source_map.insert(source_file))
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!CATEGORY language extension](https://www.ldraw.org/article/340.html#category).
#[derive(Debug, PartialEq, Clone)]
pub struct CategoryCmd {
    /// Category name.
    pub category: String,
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
/// [!KEYWORDS language extension](https://www.ldraw.org/article/340.html#keywords).
#[derive(Debug, PartialEq, Clone)]
pub struct KeywordsCmd {
    /// List of keywords.
    pub keywords: Vec<String>,
}

/// Finish for color definitions ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
pub enum MaterialFinish {
    Glitter(GlitterMaterial),
    Speckle(SpeckleMaterial),
    Other(String),
}

/// Grain size variants for the optional MATERIAL part of color definition
/// ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq, Clone)]
pub enum GrainSize {
    Size(f32),
    MinMaxSize((f32, f32)),
}

/// Glitter material definition of a color definition
/// ([!COLOUR language extension](https://www.ldraw.org/article/299.html)).
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
pub struct CommentCmd {
    /// Comment content, excluding the command identififer `0` and the optional comment marker `//`.
    pub text: String,
}

impl CommentCmd {
    pub fn new(text: &str) -> CommentCmd {
        CommentCmd {
            text: text.to_string(),
        }
    }
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) FILE start.
/// [MPD Extension[(https://www.ldraw.org/article/47.html)
#[derive(Debug, PartialEq, Clone)]
pub struct FileCmd {
    /// The filename for this file.
    pub file: String,
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) DATA.
/// [MPD Extension[(https://www.ldraw.org/article/47.html)
#[derive(Debug, PartialEq, Clone)]
pub struct DataCmd {
    /// The filename for this data file.
    pub file: String,
}

/// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) base64 data chunk.
/// [MPD Extension[(https://www.ldraw.org/article/47.html)
#[derive(Debug, PartialEq, Clone)]
pub struct Base64DataCmd {
    /// The decoded base64 data chunk.
    pub data: Vec<u8>,
}

/// Single LDraw source file loaded and optionally parsed.
#[derive(Debug, PartialEq, Clone)]
pub struct SourceFile {
    /// The relative filename of the file as resolved.
    pub filename: String,
    /// LDraw commands parsed from the raw text content of the file.
    pub cmds: Vec<Command>,
}

/// Collection of [`SourceFile`] accessible from their reference filename.
#[derive(Debug)]
pub struct SourceMap {
    /// Map of filenames to source files.
    // TODO: How to handle case sensitivity like STUD.DAT vs stud.dat?
    source_files: HashMap<String, SourceFile>,
}

impl SourceMap {
    /// Construct a new empty source map.
    pub fn new() -> SourceMap {
        SourceMap {
            source_files: HashMap::new(),
        }
    }

    /// Returns a reference to the source file corresponding to `filename`.
    pub fn get(&self, filename: &str) -> Option<&SourceFile> {
        self.source_files.get(filename)
    }

    /// Returns a mutable reference to the source file corresponding to `filename`.
    pub fn get_mut(&mut self, filename: &str) -> Option<&mut SourceFile> {
        self.source_files.get_mut(filename)
    }

    /// Inserts a new source file into the collection.
    /// Returns a copy of the filename of `source_file`
    /// or the filename of the main file for multi-part documents (MPD).
    pub fn insert(&mut self, source_file: SourceFile) -> String {
        // The MPD extension allows .ldr or .mpd files to contain multiple files.
        // Add each of these so that they can be resolved by subfile commands later.
        let files = split_files(&source_file.cmds);

        if files.is_empty() {
            // TODO: More cleanly handle the fact that not all files have 0 FILE commands.
            self.source_files
                .insert(source_file.filename.clone(), source_file.clone());
            source_file.filename
        } else {
            // The first block is the "main model" of the file.
            let main_model_name = files[0].filename.clone();
            for file in files {
                self.source_files.insert(file.filename.clone(), file);
            }
            main_model_name
        }
    }

    fn queue_subfiles(&self, source_file: &SourceFile, stack: &mut Vec<FileRef>) {
        let referer_filename = source_file.filename.clone();

        for cmd in &source_file.cmds {
            if let Command::SubFileRef(sfr_cmd) = cmd {
                // Queue this file for loading if we haven't already.
                if self.get(&sfr_cmd.file).is_none() {
                    trace!(
                        "Queuing unresolved subfile ref in {} -> {}",
                        referer_filename,
                        sfr_cmd.file
                    );
                    stack.push(FileRef {
                        filename: sfr_cmd.file.clone(),
                        referer_filename: referer_filename.clone(),
                    });
                }
            }
        }
    }
}

fn split_files(commands: &[Command]) -> Vec<SourceFile> {
    commands
        .iter()
        .enumerate()
        .filter_map(|(i, c)| match c {
            Command::File(file_cmd) => Some((i, file_cmd)),
            _ => None,
        })
        .map(|(file_start, file_cmd)| {
            // Each file block starts with a FILE command.
            // The block continues until the next NOFILE or FILE command.
            // TODO: Is there a cleaner way of expressing this?
            let subfile = &commands[file_start..];
            // Start from 1 to ignore the current file command.
            let subfile_end = subfile
                .iter()
                .skip(1)
                .position(|c| matches!(c, Command::File(_) | Command::NoFile));
            let subfile_cmds = if let Some(subfile_end) = subfile_end {
                // Add one here since we skip the first FILE command.
                subfile[..subfile_end + 1].to_vec()
            } else {
                subfile.to_vec()
            };
            SourceFile {
                filename: file_cmd.file.clone(),
                cmds: subfile_cmds,
            }
        })
        .collect()
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

/// [Line Type 1](https://www.ldraw.org/article/218.html#lt1) LDraw command:
/// Reference a sub-file from the current file.
#[derive(Debug, PartialEq, Clone)]
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
    pub file: String,
}

/// [Line Type 2](https://www.ldraw.org/article/218.html#lt2) LDraw command:
/// Draw a segment between 2 vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct LineCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the segment.
    pub vertices: [Vec3; 2],
}

/// [Line Type 3](https://www.ldraw.org/article/218.html#lt3) LDraw command:
/// Draw a triangle between 3 vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct TriangleCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the triangle.
    pub vertices: [Vec3; 3],
}

/// [Line Type 4](https://www.ldraw.org/article/218.html#lt4) LDraw command:
/// Draw a quad between 4 vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct QuadCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the quad. In theory they are guaranteed to be coplanar according to the LDraw
    /// specification, although no attempt is made to validate this property.
    pub vertices: [Vec3; 4],
}

/// [Line Type 5](https://www.ldraw.org/article/218.html#lt5) LDraw command:
/// Draw an optional segment between two vertices, aided by 2 control points.
#[derive(Debug, PartialEq, Clone)]
pub struct OptLineCmd {
    /// Color code of the primitive.
    pub color: u32,
    /// Vertices of the segment.
    pub vertices: [Vec3; 2],
    /// Control points of the segment.
    pub control_points: [Vec3; 2],
}

/// Types of commands contained in a LDraw file.
#[derive(Debug, PartialEq, Clone)]
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
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [MPD language extension](https://www.ldraw.org/article/47.html).
    File(FileCmd),
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [MPD language extension](https://www.ldraw.org/article/47.html).
    NoFile,
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [MPD language extension](https://www.ldraw.org/article/47.html).
    Data(DataCmd),
    /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) META command:
    /// [MPD language extension](https://www.ldraw.org/article/47.html).
    Base64Data(Base64DataCmd),
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
        assert_eq!(
            filename(b"  asdkwl.ldr   \r\n"),
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
            file: "aaaaaaddd".to_string(),
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
            file: "aa/aaaaddd".to_string(),
        });
        assert_eq!(
            read_line(b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"),
            Ok((&b""[..], res))
        );
    }

    #[test]
    fn test_meta_data() {
        let res = Command::Data(DataCmd {
            file: "data.bin".to_string(),
        });
        assert_eq!(read_line(b"0 !DATA data.bin"), Ok((&b""[..], res)));
    }

    #[test]
    fn test_base64_data() {
        let res = Command::Base64Data(Base64DataCmd {
            data: b"Hello World!".to_vec(),
        });
        assert_eq!(read_line(b"0 !: SGVsbG8gV29ybGQh"), Ok((&b""[..], res)));
    }

    #[test]
    fn test_file_cmd() {
        let res = Command::File(FileCmd {
            file: "submodel".to_string(),
        });
        assert_eq!(meta_cmd(b"FILE submodel"), Ok((&b""[..], res)));
    }

    #[test]
    fn test_nofile_cmd() {
        let res = Command::NoFile;
        assert_eq!(meta_cmd(b"NOFILE"), Ok((&b""[..], res)));
    }

    #[test]
    fn test_split_mpd_files() {
        let commands = vec![
            Command::File(FileCmd {
                file: "a".to_string(),
            }),
            Command::SubFileRef(SubFileRefCmd {
                color: 16,
                pos: Vec3::new(0.0, 0.0, 0.0),
                row0: Vec3::new(1.0, 0.0, 0.0),
                row1: Vec3::new(0.0, 1.0, 0.0),
                row2: Vec3::new(0.0, 0.0, 1.0),
                file: "1.dat".to_string(),
            }),
            Command::NoFile,
            Command::File(FileCmd {
                file: "b".to_string(),
            }),
            Command::SubFileRef(SubFileRefCmd {
                color: 16,
                pos: Vec3::new(0.0, 0.0, 0.0),
                row0: Vec3::new(1.0, 0.0, 0.0),
                row1: Vec3::new(0.0, 1.0, 0.0),
                row2: Vec3::new(0.0, 0.0, 1.0),
                file: "2.dat".to_string(),
            }),
            Command::NoFile,
        ];
        let subfiles = split_files(&commands);
        assert_eq!(
            vec![
                SourceFile {
                    filename: "a".to_string(),
                    cmds: commands[0..2].to_vec()
                },
                SourceFile {
                    filename: "b".to_string(),
                    cmds: commands[3..5].to_vec()
                }
            ],
            subfiles
        );
    }

    #[test]
    fn test_split_mpd_files_just_file_commands() {
        let commands = vec![
            Command::File(FileCmd {
                file: "a".to_string(),
            }),
            Command::SubFileRef(SubFileRefCmd {
                color: 16,
                pos: Vec3::new(0.0, 0.0, 0.0),
                row0: Vec3::new(1.0, 0.0, 0.0),
                row1: Vec3::new(0.0, 1.0, 0.0),
                row2: Vec3::new(0.0, 0.0, 1.0),
                file: "1.dat".to_string(),
            }),
            Command::File(FileCmd {
                file: "b".to_string(),
            }),
            Command::SubFileRef(SubFileRefCmd {
                color: 16,
                pos: Vec3::new(0.0, 0.0, 0.0),
                row0: Vec3::new(1.0, 0.0, 0.0),
                row1: Vec3::new(0.0, 1.0, 0.0),
                row2: Vec3::new(0.0, 0.0, 1.0),
                file: "2.dat".to_string(),
            }),
        ];

        let subfiles = split_files(&commands);
        assert_eq!(
            vec![
                SourceFile {
                    filename: "a".to_string(),
                    cmds: commands[0..2].to_vec()
                },
                SourceFile {
                    filename: "b".to_string(),
                    cmds: commands[2..].to_vec()
                }
            ],
            subfiles
        );
    }

    #[test]
    fn test_parse_raw_mpd() {
        // Test various language extensions.
        // Example taken from https://www.ldraw.org/article/47.html
        let ldr_contents = b"0 FILE main.ldr
        1 7 0 0 0 1 0 0 0 1 0 0 0 1 819.dat
        1 4 80 -8 70 1 0 0 0 1 0 0 0 1 house.ldr
        1 4 -70 -8 20 0 0 -1 0 1 0 1 0 0 house.ldr
        1 4 50 -8 -20 0 0 -1 0 1 0 1 0 0 house.ldr
        1 4 0 -8 -30 1 0 0 0 1 0 0 0 1 house.ldr
        1 4 -20 -8 70 1 0 0 0 1 0 0 0 1 house.ldr
        
        0 FILE house.ldr
        1 16 0 0 0 1 0 0 0 1 0 0 0 1 3023.dat
        1 16 0 -24 0 1 0 0 0 1 0 0 0 1 3065.dat
        1 16 0 -48 0 1 0 0 0 1 0 0 0 1 3065.dat
        1 16 0 -72 0 0 0 -1 0 1 0 1 0 0 3044b.dat
        1 4 0 -22 -10 1 0 0 0 0 -1 0 1 0 sticker.ldr
        
        0 FILE sticker.ldr
        0 UNOFFICIAL PART
        0 BFC CERTIFY CCW
        1 16   0 -0.25 0   20 0 0   0 0.25 0   0 0 30   box5.dat
        0 !TEXMAP START PLANAR   -20 -0.25 30   20 -0.25 30   -20 -0.25 -30   sticker.png
        4 16   -20 -0.25 30   -20 -0.25 -30   20 -0.25 -30   20 -0.25 30
        0 !TEXMAP END
        
        0 !DATA sticker.png
        0 !: iVBORw0KGgoAAAANSUhEUgAAAFAAAAB4CAIAAADqjOKhAAAAAXNSR0IArs4c6QAAAARnQU1BAACx
        0 !: jwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAEUSURBVHhe7du9DcIwFABhk5WgQLSsQM0UjMEU
        0 !: 1BQsQIsoYAt6NkAYxQV/JQ7WvfuKkFTR6UmOFJzR9bJLkXTlNwyD6QymM5ju5Tl8m67KGUt3XJcz
        0 !: J/yY8HZ/6C8BFvNZPoaesMF0BtMZTGcwncF0BtMZTGcwncF0BtMZTGcwnf8t0bmLh85gOoPpDKYz
        0 !: mM5gOoPpDKYzmM5gunDBf3tN+/zqNKt367cbOeGUTstxf1nJZHPOx68T/u3XB5/7/zMXLTqD6Qym
        0 !: M5jOYDqD6QymM5jOYDqD6QymM5jOYDqD6QymM5jOYLpwwW3t8ajBXTxtTHgwLlp0BtMZTGcwncF0
        0 !: BtMZTNfKZzyDiT3hCFy06IIFp3QH/CBMh66aBy4AAAAASUVORK5CYII=
        ";

        let commands = parse_raw(ldr_contents).unwrap();
        // TODO: Check the actual commands.
        assert_eq!(28, commands.len());
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
            file: "aa/aaaaddd".to_string(),
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
            file: "aa/aaaaddd".to_string(),
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
            file: "aa/aaaaddd".to_string(),
        });
        let cmd1 = Command::SubFileRef(SubFileRefCmd {
            color: 16,
            pos: Vec3::new(0.0, 0.0, 0.0),
            row0: Vec3::new(1.0, 0.0, 0.0),
            row1: Vec3::new(0.0, 1.0, 0.0),
            row2: Vec3::new(0.0, 0.0, 1.0),
            file: "aa/aaaaddd".to_string(),
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
            cmds: vec![Command::Triangle(TriangleCmd {
                color: 2,
                vertices: [
                    Vec3::new(0.0, 0.0, 0.0),
                    Vec3::new(1.0, 0.0, 0.0),
                    Vec3::new(0.0, 1.0, 0.0),
                ],
            })],
        };
        source_map.insert(source_file);
        let s = SourceFile {
            filename: "toto".to_string(),
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
                    file: "tata".to_string(),
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
        source_map.insert(s);
        let source_file = source_map.get("toto").unwrap();
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
