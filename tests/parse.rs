extern crate weldr;

use weldr::{
  read_lines,
  CommandType,
  SubFileRefCmd,
  Vec3
};

// LDR parsing

#[test]
fn parse_file_ref() {
  let ldr = b"1 16 0 0 0 1 0 0 0 1 0 0 0 1 s/6143.dat";
  let data = read_lines(ldr);
  let res = CommandType::SubFileRef(SubFileRefCmd{
    color: 16,
    pos: Vec3{ x: 0.0, y: 0.0, z: 0.0 },
    row0: Vec3{ x: 1.0, y: 0.0, z: 0.0 },
    row1: Vec3{ x: 0.0, y: 1.0, z: 0.0 },
    row2: Vec3{ x: 0.0, y: 0.0, z: 1.0 },
    file: "s/6143.dat"
  });
  assert_eq!(data, Ok((&b""[..], vec![res])));
}