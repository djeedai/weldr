extern crate weldr;

use weldr::{error::ResolveError, CommandType, FileRefResolver, SourceFile, SubFileRef};

use std::{cell::RefCell, collections::HashMap, rc::Rc};

fn print_rec(source_file: &SourceFile, indent: usize) {
    println!("{}{}", " ".repeat(indent), source_file.filename);
    for cmd in &source_file.cmds {
        if let CommandType::SubFileRef(sfr_cmd) = cmd {
            match &sfr_cmd.file {
                SubFileRef::ResolvedRef(resolved_file) => {
                    print_rec(&resolved_file.borrow(), indent + 2);
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
fn get_resolved_subfile_ref(cmd: &CommandType) -> Option<Rc<RefCell<SourceFile>>> {
    match cmd {
        CommandType::SubFileRef(sfr_cmd) => match &sfr_cmd.file {
            SubFileRef::ResolvedRef(source_file) => Some(Rc::clone(source_file)),
            _ => None,
        },
        _ => None,
    }
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
    let mut source_map = HashMap::new();
    let root_file = weldr::parse("root.ldr", &memory_resolver, &mut source_map).unwrap();
    let root_file = &root_file.borrow();
    assert_eq!(3, root_file.cmds.len());
    let file0 = &get_resolved_subfile_ref(&root_file.cmds[0]).unwrap();
    assert_eq!(file0.borrow().filename, "a.ldr");
    assert_eq!(1, file0.borrow().cmds.len());
    let file1 = &get_resolved_subfile_ref(&root_file.cmds[1]).unwrap();
    assert_eq!(file1.borrow().filename, "b.ldr");
    assert_eq!(2, file1.borrow().cmds.len());
    let file2 = &get_resolved_subfile_ref(&root_file.cmds[2]).unwrap();
    assert_eq!(file2.borrow().filename, "a.ldr");
    assert_eq!(1, file2.borrow().cmds.len());
    assert_eq!(file0, file2);
    let file1b = &get_resolved_subfile_ref(&file1.borrow().cmds[1]).unwrap();
    assert_eq!(file1b.borrow().filename, "a.ldr");
    assert_eq!(file0, file1b);
    print_rec(&root_file, 0);
}
