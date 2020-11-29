extern crate weldr;

use weldr::{error::ResolveError, Command, FileRefResolver, SourceFileRef, SourceMap, SubFileRef};

use std::collections::HashMap;

fn print_rec(source_map: &SourceMap, source_file_ref: SourceFileRef, indent: usize) {
    let source_file = source_file_ref.get(source_map);
    println!("{}{}", " ".repeat(indent), source_file.filename);
    for cmd in &source_file.cmds {
        if let Command::SubFileRef(sfr_cmd) = cmd {
            match &sfr_cmd.file {
                SubFileRef::ResolvedRef(resolved_file) => {
                    print_rec(source_map, *resolved_file, indent + 2);
                }
                SubFileRef::UnresolvedRef(filename) => {
                    println!("Unresolved ref: {}", filename);
                }
            }
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
    fn new() -> MemoryResolver {
        MemoryResolver {
            file_map: HashMap::new(),
        }
    }

    /// Add a new file and its content to the internal cache.
    fn add(&mut self, filename: &str, content: &[u8]) {
        self.file_map.insert(filename.to_string(), content.to_vec());
    }
}

impl FileRefResolver for MemoryResolver {
    fn resolve(&self, filename: &str) -> Result<Vec<u8>, ResolveError> {
        match self.file_map.get(filename) {
            Some(file) => Ok(file.clone()),
            None => Err(ResolveError::new_raw(filename)),
        }
    }
}

/// Get the sub-file reference command out of a given command.
fn get_resolved_subfile_ref(cmd: &Command) -> Option<SourceFileRef> {
    match cmd {
        Command::SubFileRef(sfr_cmd) => match &sfr_cmd.file {
            SubFileRef::ResolvedRef(source_file_ref) => Some(*source_file_ref),
            _ => None,
        },
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
    memory_resolver.add("root.ldr", b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 b.ldr\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr");
    memory_resolver.add("a.ldr", b"4 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0");
    memory_resolver.add(
        "b.ldr",
        b"4 16 1 1 0 0.9239 1 0.3827 0.9239 0 0.3827 1 0 0\n1 16 0 0 0 1 0 0 0 1 0 0 0 1 a.ldr",
    );
    let mut source_map = weldr::SourceMap::new();
    let root_file_ref = weldr::parse("root.ldr", &memory_resolver, &mut source_map).unwrap();
    let root_file = root_file_ref.get(&source_map);
    assert_eq!(3, root_file.cmds.len());
    let file0 = get_resolved_subfile_ref(&root_file.cmds[0]).unwrap();
    assert_eq!(file0.get(&source_map).filename, "a.ldr");
    assert_eq!(1, file0.get(&source_map).cmds.len());
    let file1 = get_resolved_subfile_ref(&root_file.cmds[1]).unwrap();
    assert_eq!(file1.get(&source_map).filename, "b.ldr");
    assert_eq!(2, file1.get(&source_map).cmds.len());
    let file2 = get_resolved_subfile_ref(&root_file.cmds[2]).unwrap();
    assert_eq!(file2.get(&source_map).filename, "a.ldr");
    assert_eq!(1, file2.get(&source_map).cmds.len());
    assert_eq!(file0, file2);
    let file1b = get_resolved_subfile_ref(&file1.get(&source_map).cmds[1]).unwrap();
    assert_eq!(file1b.get(&source_map).filename, "a.ldr");
    assert_eq!(file0, file1b);
    print_rec(&source_map, root_file_ref, 0);
}
