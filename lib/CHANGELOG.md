# Change Log

## 0.5.0

### Added

- Added support for multi-part documents (MPD extension).

### Changed

- Bumped minimum rust version (MSRV) to 1.61.

### Fixed

- Fixed a bug in hybrid file parsing, which discarded some drawing commands. (#32)

## 0.4.0

Project relicensed to a dual license MIT or Apache 2.0. See `LICENSE-APACHE2` and `LICENSE-MIT`.

## 0.3.1

### Changed

- Logging is now done via the `log` crate (_e.g._ `debug!()`) instead of direct `println!()` or `eprintln!()`.

## 0.3.0

### Added

- Add error handling via new `Error` enum.
- Add iterators to `SourceFile` to iterate eihter on all drawing commands recursively (`iter()`) or on local commands only (`local_iter()`).
- Add `DrawContext` to provide current transformation matrix and current color while iterating into sub-file references.

### Removed

- `cgmath` feature has been removed; the `cgmath` dependency is now mandatory.

### Changed

- Replaced ref-counted `Rc<RefCell<SourceFile>>` with index-based `SourceFileRef`.

### Fixed

- Handle multiple spaces between fields in drawing commands.
- Handle empty comment commands.

## 0.2.0

### Added

- Implement all draw commands (line type 2 to 5).
- Implement !CATEGORY and !KEYWORDS meta-commands.
- Implement !COLOUR meta-command.
- Feature `"cgmath"` (default) enables `Vec3` being an alias for `cgmath::Vector3<f32>`, for better interop with the `cgmath` crate.
- Add recursive parsing of a file and its sub-file references via the higher-level `parse()` function.

### Removed

- Generic command type `Cmd`; unknown meta-commands fall back to comments (`CommentCmd`).

### Changed

- Rename `read_lines()` to `parse_raw()`.

## 0.1.0

### Added

- Stub library to parse LDraw (`.ldr`) files.
