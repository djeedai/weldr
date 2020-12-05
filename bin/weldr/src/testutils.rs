#![cfg(test)]

use std::path::PathBuf;

/// Setup a test folder for the given test, ensuring the folder exists and is empty.
/// Then change the current directory to the the path and return it.
pub(crate) fn setup_test_folder(test_name: &str) -> PathBuf {
    // Ensure test dir exists and is empty
    let mut test_path = std::env::temp_dir();
    test_path.push("weldr/tests");
    test_path.push(test_name);
    if test_path.is_dir() {
        std::fs::remove_dir(&test_path).unwrap_or_default();
    }
    std::fs::create_dir_all(&test_path).unwrap_or_default();

    // Change current directory to test folder.
    std::env::set_current_dir(&test_path).unwrap();

    test_path
}
