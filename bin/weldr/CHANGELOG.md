# Change Log

## 0.4.0

### Changed

- Upgraded to `weldr` v0.5 for MPD support.
- Bumped minimum rust version (MSRV) to 1.61.

## 0.3.0

Project relicensed to a dual license MIT or Apache 2.0. See `LICENSE-APACHE2` and `LICENSE-MIT`.

## 0.2.0

### Added

- Improved support for terminal, with automatic detection of interactive terminal (TTY), and use of with emoji and color, respectively disabled with `--no-emoji` and `--no-color` (which implies `--no-emoji`).

### Changed

- `convert` command has more intuitive syntax `[OPTIONS] <FORMAT> <INPUT>`, _e.g._ `weldr convert gltf input.ldr`. This effectively swaps the `<FORMAT>` and `<INPUT>`.

### Fixed

- The `--output` option is now correctly handled and allows specifying the output filename. If absent, output goes to the standard output, allowing shell piping.

## 0.1.0

### Added

- Base executable `weldr` to manipulate LDraw files.
- `convert` command to convert an LDraw file into another format.
- `gltf` format for `convert`, to convert the model into the glTF 2.0 format.
