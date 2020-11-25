# Change Log

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
