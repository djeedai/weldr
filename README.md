# weldr, the link between your favorite building blocks and Rust

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io Version](https://img.shields.io/crates/v/weldr.svg)](https://crates.io/crates/weldr)
[![CI](https://github.com/djeedai/weldr/workflows/CI/badge.svg)](https://github.com/djeedai/weldr/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/djeedai/weldr/badge.svg?branch=main)](https://coveralls.io/github/djeedai/weldr?branch=main)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.47.0+-lightgray.svg)](#rust-version-requirements)

weldr is a Rust library to manipulate [LDraw](https://www.ldraw.org/) files ([format specification](https://www.ldraw.org/article/218.html)), which are files describing 3D models of [LEGO®](http://www.lego.com)* pieces.

weldr allows building command-line tools and applications leveraging [the fantastic database of pieces](https://www.ldraw.org/cgi-bin/ptlist.cgi) contributed by the LDraw community.

## Example

Parse a single `.ldr` line containing a [file reference command (line type 1)](https://www.ldraw.org/article/218.html#lt1):

```rust
extern crate weldr;
use weldr::read_lines;

fn main() {}

#[test]
fn parse_file_ref() {
  let ldr = b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 s/6143.dat";
  let data = read_lines(ldr);
  let res = CommandType::SubFileRef(SubFileRefCmd{
    color: 16,
    pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
    row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
    row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
    row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
    file: "s/6143.dat"
  });
  assert_eq!(data, Ok((&b""[..], vec![res])));
}
```

The `read_lines()` function can be used to parse an entire file too.

## Documentation

[Reference documentation](https://docs.rs/weldr)

## Rust version requirements

weldr was only tested so far with  **Rustc version 1.47** although older versions may work, but for lack of time have never been tested.

## Installation

weldr is available on [crates.io](https://crates.io/crates/weldr) and can be included in your Cargo enabled project like this:

```toml
[dependencies]
weldr = "0.1"
```

Then include it in your code like this:

```rust,ignore
#[macro_use]
extern crate weldr;
```

## Technical features

weldr leverages the [nom parser combinator library](https://crates.io/crates/nom) to efficiently and reliably parse LDraw files, and transform them into in-memory data structures for consumption. All parsing is done on `&[u8]` input expected to contain [specification](https://www.ldraw.org/article/218.html)-compliant LDraw content. In particular, this means:

- UTF-8 encoded input
- Both DOS/Windows `<CR><LF>` and Unix `<LF>` line termination accepted

## Copyrights

The current code repository is licensed under the MIT license.

LDraw™ is a trademark owned and licensed by the Estate of James Jessiman, which does not sponsor, endorse, or authorize this project.

LEGO® is a registered trademark of the LEGO Group, which does not sponsor, endorse, or authorize this project.
