#![cfg(test)]

use std::path::PathBuf;

/// RAII-style test folder helper, which deletes the entire folder
/// once it goes out of scope.
pub(crate) struct TestFolder {
    path: PathBuf,
}

impl TestFolder {
    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }
}

impl std::ops::Drop for TestFolder {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.path).unwrap_or_default();
    }
}

/// Setup a test folder for the given test, ensuring the folder exists and is empty.
/// Then change the current directory to the the path and return it.
pub(crate) fn setup_test_folder(test_name: &str) -> TestFolder {
    // Ensure test dir exists and is empty
    let mut test_path = std::fs::canonicalize(std::env::temp_dir()).unwrap();
    test_path.push("weldr");
    test_path.push("tests");
    test_path.push(test_name);
    if test_path.is_dir() {
        std::fs::remove_dir_all(&test_path).unwrap_or_default();
    }
    std::fs::create_dir_all(&test_path).unwrap_or_default();

    // Change current directory to test folder.
    std::env::set_current_dir(&test_path).unwrap();

    TestFolder { path: test_path }
}
