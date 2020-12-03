# 👨‍🏭 weldr, the link between your favorite building blocks and Rust 🧱

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io Version](https://img.shields.io/crates/v/weldr.svg)](https://crates.io/crates/weldr)
[![CI](https://github.com/djeedai/weldr/workflows/CI/badge.svg?branch=main)](https://github.com/djeedai/weldr/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/djeedai/weldr/badge.svg?branch=main)](https://coveralls.io/github/djeedai/weldr?branch=main)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.44.0+-lightgray.svg)](#rust-version-requirements)

weldr is a Rust library and command-line tool to manipulate [LDraw](https://www.ldraw.org/) files ([format specification](https://www.ldraw.org/article/218.html)), which are files describing 3D models of [LEGO®](http://www.lego.com)* pieces.

The **[📦 `weldr`](https://crates.io/crates/weldr) crate** contains the library which allows building command-line tools and applications leveraging [the fantastic database of pieces](https://www.ldraw.org/cgi-bin/ptlist.cgi) contributed by the LDraw community.

_Note: For the binary command-line tool ⚙ `weldr`, see the [📦 `weldr-bin` crate](https://crates.io/crates/weldr-bin) instead._

## Example

Use the `weldr` crate to parse the content of a single LDraw file containing 2 commands:

```rust
extern crate weldr;

use weldr::{parse_raw, CommandType, CommentCmd, LineCmd, Vec3};

fn main() {}

#[test]
fn test_weldr() {
  let cmd0 = CommandType::Comment(CommentCmd{ text: "this is a comment".to_string() });
  let cmd1 = CommandType::Line(LineCmd{
    color: 16,
    vertices: [
      Vec3{ x: 0.0, y: 0.0, z: 0.0 },
      Vec3{ x: 1.0, y: 1.0, z: 1.0 }
    ]
  });
  assert_eq!(parse_raw(b"0 this is a comment\n2 16 0 0 0 1 1 1"), vec![cmd0, cmd1]);
}
```

## Documentation

[Reference documentation](https://docs.rs/weldr)

## Rust version requirements

weldr is tested with `rustc` **version 1.44**, **stable**, and **beta**, although older versions may work, but have never been tested.

## Installation

weldr is available on [crates.io](https://crates.io/crates/weldr) and can be included in your Cargo enabled project like this:

```toml
[dependencies]
weldr = "0.3"
```

Then include it in your code like this:

```rust,ignore
extern crate weldr;
```

## Technical features

weldr leverages the [nom parser combinator library](https://crates.io/crates/nom) to efficiently and reliably parse LDraw files, and transform them into in-memory data structures for consumption. All parsing is done on `&[u8]` input expected to contain [specification](https://www.ldraw.org/article/218.html)-compliant LDraw content. In particular, this means:

- UTF-8 encoded input
- Both DOS/Windows `<CR><LF>` and Unix `<LF>` line termination accepted

## Copyrights

The current code repository is licensed under the MIT license.

LDraw™ is a trademark owned and licensed by the Estate of James Jessiman, which does not sponsor, endorse, or authorize this project.

*LEGO® is a registered trademark of the LEGO Group, which does not sponsor, endorse, or authorize this project.
