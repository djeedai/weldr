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

use std::str;
use std::str::FromStr;
use std::str::from_utf8;
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

// LDraw File Format Specification
// https://www.ldraw.org/article/218.html

fn alpha(s: &[u8]) -> IResult<&[u8], &[u8]> {
  take_while1(is_alphabetic)(s)
}

fn alphanum(s: &[u8]) -> IResult<&[u8], &[u8]> {
  take_while1(is_alphanumeric)(s)
}

fn cmd_id(s: &[u8]) -> IResult<&[u8], &[u8]> {
  take_while1(is_digit)(s)
}

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

// Read the command ID and swallow the following space
fn read_cmd_id_str(input: &[u8]) -> IResult<&[u8], &[u8]> {
  terminated(take_while1(is_digit), sp)(input)
}

// fn read_cmd_id(input: &[u8]) -> IResult<&[u8], i32> {
//   match take_while1(is_digit)(input) {
//     Ok((output, id_s)) => {
//       let id_str = String::from_utf8_lossy(id_s);
//       Ok((output, id_str.parse::<i32>().unwrap()))
//     },
//     Err(e) => Err(e)
//   }
// }

named!(meta_cmd<CommandType>,
  do_parse!(
    content: take_not_cr_or_lf >> (
      CommandType::Command(Cmd{ id: 0, content: std::str::from_utf8(content).unwrap() })
    )
  )
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

//named!(color_id<i32>, parse_to!(i32));

// fn parse_i32(s: &[u8]) -> i32 {
//   FromStr::from_str(from_utf8(s).unwrap()).unwrap()
// }

// named!(color_id<i32>,
//   map!(digit1, parse_i32)
// );

named!(color_id<i32>,
  map_res!(
    map_res!(digit1, str::from_utf8),
    str::parse::<i32>
  )
);

// named!(filename<&str>,
//   let s = match map!(take_while1!(is_digit), str::from_utf8) {
//     Ok(v) => v,
//     Err(e) => e
//   }
// );

// fn filename(input: &[u8]) -> IResult<&[u8], &str> {
//   let s = match str::from_utf8(input) {
//     Ok(v) => Ok(input, v),
//     Err(e) => Err(e)
//   };
//   s
// }

// fn filename(input: &[u8]) -> IResult<&[u8], &'static str> {
//   Ok((input, "testssxxxx"))
// }

// fn take_filename_char(input: &[u8]) -> IResult<&[u8], char> {
//   if input.is_empty() {
//     Err()
//   } else {
//     Ok((input, input[0]))
//   }
// }

#[inline]
fn is_filename_char(chr: u8) -> bool {
  is_alphanumeric(chr) || chr == b'/' || chr == b'\\' || chr == b'.' || chr == b'-'
}

// named!(filename_char,
//   take_while1!(is_filename_char)
// );

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
      file: file
    })
  )
)
);

fn line_cmd(input: &[u8]) -> IResult<&[u8], CommandType> {
  Ok((input, CommandType::Command(Cmd{ id: 0, content: &"" })))
}

fn tri_cmd(input: &[u8]) -> IResult<&[u8], CommandType> {
  Ok((input, CommandType::Command(Cmd{ id: 0, content: &"" })))
}

fn quad_cmd(input: &[u8]) -> IResult<&[u8], CommandType> {
  Ok((input, CommandType::Command(Cmd{ id: 0, content: &"" })))
}

fn opt_line_cmd(input: &[u8]) -> IResult<&[u8], CommandType> {
  Ok((input, CommandType::Command(Cmd{ id: 0, content: &"" })))
}

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


#[derive(Debug, PartialEq)]
pub struct Cmd<'a> {
  id: i32,
  content: &'a str
}

#[derive(Debug, PartialEq)]
pub struct Comment {
  text: str
}

/// Generic 3-component vector.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32
}

impl Vec3 {
    pub fn dot(&self, other: &Vec3) -> f32 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    pub fn normalized(&self) -> Vec3 {
        let invsqrt : f32 = self.dot(self).sqrt().recip();
        self * invsqrt 
    }
}

impl std::ops::Add<Vec3> for Vec3 {
  type Output = Vec3;

  fn add(self, other: Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl std::ops::Add<&Vec3> for Vec3 {
  type Output = Vec3;

  fn add(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl<'a> std::ops::Add<&Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn add(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl<'a> std::ops::Add<Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn add(self, other: Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl std::ops::Add<f32> for Vec3 {
  type Output = Vec3;

  fn add(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x + other,
          y: self.y + other,
          z: self.z + other
      }
  }
}

impl<'a> std::ops::Add<f32> for &'a Vec3 {
  type Output = Vec3;

  fn add(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x + other,
          y: self.y + other,
          z: self.z + other
      }
  }
}

impl std::ops::AddAssign<Vec3> for Vec3 {
  fn add_assign(&mut self, other: Vec3) {
      self.x += other.x;
      self.y += other.y;
      self.z += other.z;
  }
}

impl std::ops::AddAssign<&Vec3> for Vec3 {
  fn add_assign(&mut self, other: &Vec3) {
      self.x += other.x;
      self.y += other.y;
      self.z += other.z;
  }
}

impl<'a> std::ops::Sub<&Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn sub(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x - other.x,
          y: self.y - other.y,
          z: self.z - other.z
      }
  }
}

impl<'a> std::ops::Mul<&Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn mul(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x * other.x,
          y: self.y * other.y,
          z: self.z * other.z
      }
  }
}

impl std::ops::Mul<f32> for Vec3 {
  type Output = Vec3;

  fn mul(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x * other,
          y: self.y * other,
          z: self.z * other
      }
  }
}

impl<'a> std::ops::Mul<f32> for &'a Vec3 {
  type Output = Vec3;

  fn mul(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x * other,
          y: self.y * other,
          z: self.z * other
      }
  }
}

/// Line Type 1 LDraw command to reference a sub-file from the current file.
/// 
/// [Specification](https://www.ldraw.org/article/218.html#lt1)
#[derive(Debug, PartialEq)]
pub struct SubFileRefCmd<'a> {
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
  /// Name of referenced sub-file.
  pub file: &'a str
}

/// Types of commands contained in a LDraw file.
#[derive(Debug, PartialEq)]
pub enum CommandType<'a> {
  /// [Line Type 0](https://www.ldraw.org/article/218.html#lt0) comment or META command.
  Command(Cmd<'a>),
  /// [Line Type 1](https://www.ldraw.org/article/218.html#lt1) sub-file reference.
  SubFileRef(SubFileRefCmd<'a>),
  /// [Line Type 2](https://www.ldraw.org/article/218.html#lt2) segment.
  Line(),
  /// [Line Type 3](https://www.ldraw.org/article/218.html#lt3) triangle.
  Triangle(),
  /// [Line Type 4](https://www.ldraw.org/article/218.html#lt4) quadrilateral.
  Quad(),
  /// [Line Type 5](https://www.ldraw.org/article/218.html#lt5) optional line.
  OptLine()
}

/// Resolver trait for file references.
pub trait FileRefResolver {
  /// Resolve the given file ref name and return the content of the file.
  fn resolve(&self, name: &str) -> &[u8];
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
  use nom::error::ErrorKind::AlphaNumeric;

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
  fn test_meta_cmd() {
    let res = CommandType::Command(Cmd{ id: 0, content: &"test of metacommand" });
    assert_eq!(meta_cmd(b"test of metacommand"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_file_ref_cmd() {
    let res = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: "aaaaaaddd"
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
  fn test_read_line_cmd() {
    let res = CommandType::Command(Cmd{ id: 0, content: &"this doesn't matter" });
    assert_eq!(read_line(b"0 this doesn't matter"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_line_subfileref() {
    let res = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: "aa/aaaaddd"
    });
    assert_eq!(read_line(b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"), Ok((&b""[..], res)));
  }

  #[test]
  fn test_read_lines() {
    let cmd0 = CommandType::Command(Cmd{ id: 0, content: &"this doesn't matter" });
    let cmd1 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: "aa/aaaaddd"
    });
    assert_eq!(read_lines(b"\n0 this doesn't matter\n\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"), Ok((&b""[..], vec![cmd0, cmd1])));
    
    let cmd0 = CommandType::Command(Cmd{ id: 0, content: &"this doesn't \"matter\"" });
    let cmd1 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: "aa/aaaaddd"
    });
    assert_eq!(read_lines(b"\r\n0 this doesn't \"matter\"\r\n\r\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd\n"), Ok((&b""[..], vec![cmd0, cmd1])));

    let cmd0 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: "aa/aaaaddd"
    });
    let cmd1 = CommandType::SubFileRef(SubFileRefCmd{
      color: 16,
      pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
      row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
      row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
      file: "aa/aaaaddd"
    });
    assert_eq!(read_lines(b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 aa/aaaaddd"), Ok((&b""[..], vec![cmd0, cmd1])));
  }
}
