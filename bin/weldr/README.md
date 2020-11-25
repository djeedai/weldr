# 👨‍🏭 weldr, the link between your favorite building blocks and Rust 🧱

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io Version](https://img.shields.io/crates/v/weldr-bin.svg)](https://crates.io/crates/weldr-bin)
[![CI](https://github.com/djeedai/weldr/workflows/CI/badge.svg?branch=main)](https://github.com/djeedai/weldr/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/djeedai/weldr/badge.svg?branch=main)](https://coveralls.io/github/djeedai/weldr?branch=main)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.44.0+-lightgray.svg)](#rust-version-requirements)

weldr is a Rust library and command-line tool to manipulate [LDraw](https://www.ldraw.org/) files ([format specification](https://www.ldraw.org/article/218.html)), which are files describing 3D models of [LEGO®](http://www.lego.com)* pieces.

The **[📦 `weldr-bin`](https://crates.io/crates/weldr-bin) crate** contains the weldr command-line tool ⚙ `weldr`, an executable to manipulate LDraw files and convert them to other formats (currently: glTF 2.0).

_Note: For the underlying Rust library used by this command-line tool, see the [📦 `weldr` crate](https://crates.io/crates/weldr) instead._

## Example

Convert an LDraw file to a glTF 2.0 file:

```shell
weldr convert 5-8cyli.dat gltf
```

⚙ `weldr` is a frontend for a set of subcommands to manipulate LDraw files. The full help and list of available subcommands can be displayed with:

```shell
weldr --help
```

Topical help for a specific subcommand (_e.g._ "`convert`") can be displayed with:

```shell
weldr <SUBCOMMAND> --help
```

## Documentation

[Reference documentation](https://docs.rs/weldr-bin)

## Rust version requirements

weldr is tested with `rustc` **version 1.44**, **stable**, and **beta**, although older versions may work, but have never been tested.

## Installation

### Cargo

The 📦 `welder-bin` crate is available on [crates.io](https://crates.io/crates/weldr-bin) and can be installed with cargo:

```shell
cargo install weldr-bin
```

### Binary download

Prebuilt binaries for supported platforms are available on the [GitHub Releases](https://github.com/djeedai/weldr/releases) page.

## Copyrights

The current code repository is licensed under the MIT license.

LDraw™ is a trademark owned and licensed by the Estate of James Jessiman, which does not sponsor, endorse, or authorize this project.

*LEGO® is a registered trademark of the LEGO Group, which does not sponsor, endorse, or authorize this project.
