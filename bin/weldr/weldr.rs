//! The weldr tool for LDraw format management.

mod gltf;

use structopt::StructOpt;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(StructOpt)]
struct Cli {
    /// The pattern to look for
    pattern: String,
    /// The path to the file to read
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() -> Result<(), weldr::Error> {
    println!("weldr-bin");

    let buf = gltf::Buffer{ name: None, byte_length: 32, uri: Some("buf1.glb".to_string()) };
    let asset = gltf::Asset{ version: "3.1".to_string(), min_version: None, generator: Some("weldr".to_string()), copyright: None };
    let gltf = gltf::Gltf{ asset, nodes: vec![], scenes: vec![], buffers: vec![buf], scene: None };
    let json = serde_json::to_string_pretty(&gltf);
    println!("json={:#?}", json.unwrap());

    let args = Cli::from_args();
    println!("args: pattern={}, path={}", args.pattern, args.path.to_str().unwrap());
    let cmds = weldr::parse_raw(&b"1 16 0 0 0 1 0 0 0 1 0 sub.ldr"[..])?;
    println!("cmds #{}", cmds.len());
    Ok(())
}
