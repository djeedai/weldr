# 👨‍🏭 weldr, the link between your favorite building blocks and Rust 🧱

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io Version](https://img.shields.io/crates/v/weldr.svg)](https://crates.io/crates/weldr)
[![CI](https://github.com/djeedai/weldr/workflows/CI/badge.svg?branch=main)](https://github.com/djeedai/weldr/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/djeedai/weldr/badge.svg?branch=main)](https://coveralls.io/github/djeedai/weldr?branch=main)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.44.0+-lightgray.svg)](#rust-version-requirements)

weldr is a Rust library and command-line tool to manipulate [LDraw](https://www.ldraw.org/) files ([format specification](https://www.ldraw.org/article/218.html)), which are files describing 3D models of [LEGO®](http://www.lego.com)* pieces.

This repository is a Cargo workspace composed of the following packages:

➡ [`lib/` : the weldr library (📦 `weldr` crate)](./lib)

➡ [`bin/weldr/` : the ⚙ `weldr` command-line tool (📦 `weldr-bin` crate)](./bin/weldr)

## Library

The weldr library allows building command-line tools and applications leveraging [the fantastic database of pieces](https://www.ldraw.org/cgi-bin/ptlist.cgi) contributed by the LDraw community.

Parse the content of a single LDraw file containing 2 commands:

```rust
extern crate weldr;

use weldr::{parse_raw, CommandType, CommentCmd, LineCmd, Vec3};

fn main() {}

#[test]
fn test_weldr() {
  let cmd0 = CommandType::Comment(
    CommentCmd{ text: "this is a comment".to_string() }
  );
  let cmd1 = CommandType::Line(LineCmd{
    color: 16,
    vertices: [
      Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      Vec3{ x: 1.0, y: 1.0, z: 1.0 }
    ]
  });
  assert_eq!(
    parse_raw(b"0 this is a comment\n2 16 0 0 0 1 1 1"),
    vec![cmd0, cmd1]
  );
}
```

## Command-line tool

The weldr command-line tool ⚙ `weldr` is an executable to manipulate LDraw files and convert them to other formats (currently: glTF 2.0).

Convert an LDraw file to a glTF 2.0 file:

```shell
weldr convert 5-8cyli.dat gltf
```

The format is:

```shell
weldr <COMMAND> <INPUT>
```

You can get the list of commands with `weldr --help`. Currently only the `convert` command is implemented for the `gltf` (glTF 2.0) format.

## Copyrights

The current code repository is licensed under the MIT license.

LDraw™ is a trademark owned and licensed by the Estate of James Jessiman, which does not sponsor, endorse, or authorize this project.

*LEGO® is a registered trademark of the LEGO Group, which does not sponsor, endorse, or authorize this project.
