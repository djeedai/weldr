use weldr::{error::ResolveError, Command, FileRefResolver, SourceFile, SourceMap, SubFileRefCmd};

use std::collections::HashMap;

fn print_rec(source_map: &SourceMap, filename: &str, source_file: &SourceFile, indent: usize) {
    eprintln!("{}{}", " ".repeat(indent), filename);
    for cmd in &source_file.cmds {
        if let Command::SubFileRef(sfr_cmd) = cmd {
            let resolved_file = source_map.get(&sfr_cmd.file).unwrap();
            print_rec(source_map, &sfr_cmd.file, resolved_file, indent + 2);
        }
    }
}

/// A simple in-memory file resolver where files and their content are manually
/// added before being resolved.
struct MemoryResolver {
    /// Mapping from the filename to the file content.
    file_map: HashMap<String, Vec<u8>>,
}

impl MemoryResolver {
    fn new() -> Self {
        Self {
            file_map: HashMap::new(),
        }
    }

    /// Add a new file and its content to the internal cache.
    fn add(&mut self, filename: &str, content: &[u8]) {
        self.file_map.insert(filename.to_string(), content.to_vec());
    }
}

impl FileRefResolver for MemoryResolver {
    fn resolve<P: AsRef<std::path::Path>>(&self, filename: P) -> Result<Vec<u8>, ResolveError> {
        let filename = filename.as_ref().to_string_lossy().to_string();
        match self.file_map.get(&filename) {
            Some(file) => Ok(file.clone()),
            None => Err(ResolveError::new_raw(&filename)),
        }
    }
}

/// Get the sub-file reference command out of a given command.
fn get_resolved_subfile_ref(cmd: &Command) -> Option<&SubFileRefCmd> {
    match cmd {
        Command::SubFileRef(sfr_cmd) => Some(sfr_cmd),
        _ => None,
    }
}

#[test]
fn test_memory_resolver() {
    let mut memory_resolver = MemoryResolver::new();
    memory_resolver.add("root.ldr", b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr");
    assert!(memory_resolver.resolve("root.ldr").is_ok());
    assert!(memory_resolver.resolve("a.ldr").is_err());
    assert_eq!(
        "a.ldr",
        memory_resolver.resolve("a.ldr").unwrap_err().filename
    );
}

#[test]
fn parse_recursive() {
    let mut memory_resolver = MemoryResolver::new();
    memory_resolver.add("root.ldr", b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr\n4 16 0 0 0 1 1 1 2 2 2 3 3 3\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 b.ldr\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr");
    memory_resolver.add("a.ldr", b"4 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0");
    memory_resolver.add(
        "b.ldr",
        b"4 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr",
    );
    let mut source_map = weldr::SourceMap::new();
    let root_file_name = weldr::parse("root.ldr", &memory_resolver, &mut source_map).unwrap();
    let root_file = source_map.get(&root_file_name).unwrap();
    assert_eq!(4, root_file.cmds.len());
    // Regression #32: top-level drawing commands work with sub-file references
    assert!(matches!(root_file.cmds[1], Command::Quad(_)));

    let file0 = get_resolved_subfile_ref(&root_file.cmds[0]).unwrap();
    assert_eq!(file0.file, "a.ldr");
    assert_eq!(1, source_map.get(&file0.file).unwrap().cmds.len());

    let file1 = get_resolved_subfile_ref(&root_file.cmds[2]).unwrap();
    assert_eq!(2, source_map.get(&file1.file).unwrap().cmds.len());

    let file2 = get_resolved_subfile_ref(&root_file.cmds[3]).unwrap();
    assert_eq!(1, source_map.get(&file2.file).unwrap().cmds.len());
    assert_eq!(file0, file2);

    let file1b = get_resolved_subfile_ref(&source_map.get(&file1.file).unwrap().cmds[1]).unwrap();
    assert_eq!(file0, file1b);
    print_rec(&source_map, "root.ldr", &root_file, 0);
}

#[test]
fn parse_cycle() {
    let mut memory_resolver = MemoryResolver::new();

    // Infinite recursion on a naive implementation.
    let ldr_contents = b"0 FILE a.ldr
    1 16 0 0 0 1 0 0 0 1 0 0 0 1 b.ldr

    0 FILE b.ldr
    1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr
    ";

    memory_resolver.add("root.ldr", ldr_contents);

    let mut source_map = weldr::SourceMap::new();

    weldr::parse("root.ldr", &memory_resolver, &mut source_map).unwrap();
    let file_a = source_map.get("a.ldr").unwrap();
    assert_eq!(2, file_a.cmds.len());

    let file_b = source_map.get("b.ldr").unwrap();
    assert_eq!(2, file_b.cmds.len());
}
