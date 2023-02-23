# 👨‍🏭 weldr, the link between your favorite building blocks and Rust 🧱

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Crates.io Version](https://img.shields.io/crates/v/weldr-bin.svg)](https://crates.io/crates/weldr-bin)
[![CI](https://github.com/djeedai/weldr/workflows/CI/badge.svg?branch=main)](https://github.com/djeedai/weldr/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/djeedai/weldr/badge.svg?branch=main)](https://coveralls.io/github/djeedai/weldr?branch=main)
[![Minimum rustc version](https://img.shields.io/badge/rustc-1.56.0+-lightgray.svg)](#rust-version-requirements)

weldr is a Rust library and command-line tool to manipulate [LDraw](https://www.ldraw.org/) files ([format specification](https://www.ldraw.org/article/218.html)), which are files describing 3D models of [LEGO®](http://www.lego.com)* pieces.

The **[📦 `weldr-bin`](https://crates.io/crates/weldr-bin) crate** contains the weldr command-line tool ⚙ `weldr`, an executable to manipulate LDraw files and convert them to other formats (currently: glTF 2.0).

_Note: For the underlying Rust library used by this command-line tool, see the [📦 `weldr` crate](https://crates.io/crates/weldr) instead._

## Example

Convert an LDraw file to a glTF 2.0 file:

```shell
weldr convert gltf 5-8cyli.dat --output 5-8cyli.gltf
```

⚙ `weldr` is a frontend for a set of subcommands to manipulate LDraw files. The full help and list of available subcommands can be displayed with:

```shell
weldr --help
```

Topical help for a specific subcommand (_e.g._ "`convert`") can be displayed with:

```shell
weldr <SUBCOMMAND> --help
```

Currently the `convert` subcommand is the only one available:

```shell
weldr convert [OPTIONS] <FORMAT> <INPUT>
```

## Installation

### Cargo

The 📦 `welder-bin` crate is available on [crates.io](https://crates.io/crates/weldr-bin) and can be installed with cargo:

```shell
cargo install weldr-bin
```

### Binary download

Prebuilt binaries for supported platforms are available on the [GitHub Releases](https://github.com/djeedai/weldr/releases) page.

## Offcial LDraw catalog

The official LDraw catalog of parts is available at <https://www.ldraw.org/library/updates/complete.zip>. Download the catalog and unzip it somewhere locally, then use the `--catalog-path`/`-C` option to specify the location where the root folders are located (the `p` and `parts` folders), to allow ⚙ `weldr` to resolve files and all their sub-file references. By default the current working directory is used.

Example:

```shell
> curl https://www.ldraw.org/library/updates/complete.zip --output complete.zip
> unzip complete.zip -d ./ldraw_parts
> weldr convert -C ./ldraw_parts 6143.dat gltf
```

## Rust version requirements

weldr is tested with `rustc` **version 1.56**, **stable**, and **beta**, although older versions may work, but have never been tested.

## Copyrights

The current code repository is licensed under the MIT license.

LDraw™ is a trademark owned and licensed by the Estate of James Jessiman, which does not sponsor, endorse, or authorize this project.

*LEGO® is a registered trademark of the LEGO Group, which does not sponsor, endorse, or authorize this project.
