//! GLTF writer.

#![allow(dead_code)]

use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
pub struct Gltf {
    pub asset: Asset,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub nodes: Vec<Node>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub scenes: Vec<Scene>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub buffers: Vec<Buffer>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(rename(serialize = "bufferViews"))]
    pub buffer_views: Vec<BufferView>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub accessors: Vec<Accessor>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub meshes: Vec<Mesh>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub scene: Option<u32>,
}

#[derive(Serialize, Deserialize)]
pub struct Asset {
    pub version: String,

    #[serde(rename(serialize = "minVersion"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_version: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub generator: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub copyright: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct Scene {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub nodes: Vec<u32>,
}

#[derive(Serialize, Deserialize)]
pub struct Node {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<u32>,

    #[serde(rename(serialize = "mesh"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mesh_index: Option<u32>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub matrix: Option<[f32; 16]>,
}

#[derive(Serialize, Deserialize)]
pub struct Buffer {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(rename(serialize = "byteLength"))]
    pub byte_length: u32,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct BufferView {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(rename(serialize = "buffer"))]
    pub buffer_index: u32,

    #[serde(rename(serialize = "byteLength"))]
    pub byte_length: u32,

    #[serde(rename(serialize = "byteOffset"))]
    #[serde(skip_serializing_if = "is_zero")]
    pub byte_offset: u32, // optional, but defaults to zero

    #[serde(rename(serialize = "byteStride"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_stride: Option<u32>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<u32>,
}

fn is_zero(value: &u32) -> bool {
    *value == 0
}

fn is_false(value: &bool) -> bool {
    !(*value)
}

#[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug, Clone, Copy)]
#[repr(u32)]
pub enum ComponentType {
    Byte = 5120,
    UnsignedByte = 5121,
    Short = 5122,
    UnsignedShort = 5123,
    UnsignedInt = 5125,
    Float = 5126,
}

#[derive(Serialize, Deserialize, Clone, Copy)]
pub enum AttributeType {
    #[serde(rename(serialize = "SCALAR"))]
    Scalar,

    #[serde(rename(serialize = "VEC2"))]
    Vec2,

    #[serde(rename(serialize = "VEC3"))]
    Vec3,

    #[serde(rename(serialize = "VEC4"))]
    Vec4,

    #[serde(rename(serialize = "MAT2"))]
    Mat2,

    #[serde(rename(serialize = "MAT3"))]
    Mat3,

    #[serde(rename(serialize = "MAT4"))]
    Mat4,
}

#[derive(Serialize, Deserialize)]
pub struct Accessor {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(rename(serialize = "componentType"))]
    pub component_type: ComponentType,

    pub count: u32,

    #[serde(rename(serialize = "type"))]
    pub attribute_type: AttributeType,

    #[serde(rename(serialize = "bufferView"))]
    pub buffer_view_index: u32,

    #[serde(rename(serialize = "byteOffset"))]
    #[serde(skip_serializing_if = "is_zero")]
    pub byte_offset: u32, // optional, but defaults to zero

    #[serde(skip_serializing_if = "is_false")]
    pub normalized: bool, // optional, but defaults to false

    #[serde(skip_serializing_if = "Option::is_none")]
    pub min: Option<[f32; 3]>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub max: Option<[f32; 3]>,
}

#[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum PrimitiveMode {
    Points = 0,
    Lines = 1,
    LineLoop = 2,
    LineStrip = 3,
    Triangles = 4,
    TriangleStrip = 5,
    TriangleFan = 6,
}

pub type AccessorIndex = u32;

#[derive(Serialize, Deserialize)]
pub struct Primitive {
    /// Map of indices into the `Gltf.accessors` array from the vertex attribute semantic.
    pub attributes: HashMap<String, AccessorIndex>,

    /// Index into the `Gltf.accessors` array of the accessor for indices.
    #[serde(skip_serializing_if = "is_zero")]
    pub indices: AccessorIndex, // optional, but defaults to zero

    pub mode: PrimitiveMode,
}

#[derive(Serialize, Deserialize)]
pub struct Mesh {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    pub primitives: Vec<Primitive>,
}

impl Gltf {
    pub fn new(asset: Asset) -> Self {
        Self {
            asset,
            nodes: vec![],
            scenes: vec![],
            buffers: vec![],
            buffer_views: vec![],
            accessors: vec![],
            meshes: vec![],
            scene: None,
        }
    }

    pub fn add_scene(&mut self, scene: Scene) {
        self.scenes.push(scene);
        if self.scene.is_none() {
            self.scene = Some(0);
        }
    }
}

impl Asset {
    pub fn new(version: &str) -> Self {
        Self {
            version: version.to_string(),
            min_version: None,
            generator: None,
            copyright: None,
        }
    }

    #[inline(always)]
    #[cfg(not(tarpaulin_include))] // tarpaulin doesn't understand inline I think
    pub fn min_version(mut self, min_version: &str) -> Self {
        self.min_version = Some(min_version.to_string());
        self
    }

    #[inline(always)]
    #[cfg(not(tarpaulin_include))] // tarpaulin doesn't understand inline I think
    pub fn generator(mut self, generator: &str) -> Self {
        self.generator = Some(generator.to_string());
        self
    }

    #[inline(always)]
    #[cfg(not(tarpaulin_include))] // tarpaulin doesn't understand inline I think
    pub fn copyright(mut self, copyright: &str) -> Self {
        self.copyright = Some(copyright.to_string());
        self
    }
}

impl Buffer {
    pub fn new(byte_length: u32) -> Self {
        Self {
            name: None,
            byte_length,
            uri: None,
        }
    }

    pub fn uri(mut self, uri: &str) -> Self {
        self.uri = Some(uri.to_string());
        self
    }
}

pub enum BufferTarget {
    ArrayBuffer = 34962,
    ElementArrayBuffer = 34963,
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_is_zero() {
        assert!(is_zero(&0));
        assert!(!is_zero(&1));
        assert!(!is_zero(&0xFFFFFFFF));
    }

    #[test]
    fn test_is_false() {
        assert!(is_false(&false));
        assert!(!is_false(&true));
    }

    #[test]
    fn test_asset_utils() {
        assert_eq!(
            Asset::new("2.0").min_version("1.0").min_version,
            Some("1.0".to_string())
        );
        assert_eq!(
            Asset::new("2.0").generator("weldr").generator,
            Some("weldr".to_string())
        );
        assert_eq!(
            Asset::new("2.0").copyright("(c) weldr").copyright,
            Some("(c) weldr".to_string())
        );
    }

    #[test]
    fn test_asset_add_scene() {
        let asset = Asset::new("2.0");
        let scene = Scene {
            name: None,
            nodes: vec![],
        };
        let mut gltf = Gltf::new(asset);
        gltf.add_scene(scene);
        assert_eq!(1, gltf.scenes.len());
        assert!(gltf.scene.is_some());
    }

    #[test]
    fn root() {
        let asset = Asset::new("2.0");
        let gltf = Gltf::new(asset);
        assert_eq!(
            serde_json::to_string(&gltf).unwrap(),
            "{\"asset\":{\"version\":\"2.0\"}}".to_string()
        );
    }

    #[test]
    fn asset() {
        let asset = Asset::new("2.0");
        assert_eq!(
            serde_json::to_string(&asset).unwrap(),
            "{\"version\":\"2.0\"}".to_string()
        );
        let asset = Asset::new("2.0").generator("weldr");
        assert_eq!(
            serde_json::to_string(&asset).unwrap(),
            "{\"version\":\"2.0\",\"generator\":\"weldr\"}".to_string()
        );
        let asset = Asset::new("2.0").min_version("1.0");
        assert_eq!(
            serde_json::to_string(&asset).unwrap(),
            "{\"version\":\"2.0\",\"minVersion\":\"1.0\"}".to_string()
        );
        let asset = Asset::new("2.0").copyright("(c) weldr");
        assert_eq!(
            serde_json::to_string(&asset).unwrap(),
            "{\"version\":\"2.0\",\"copyright\":\"(c) weldr\"}".to_string()
        );
    }

    #[test]
    fn buffer() {
        let buffer = Buffer::new(32);
        assert_eq!(32, buffer.byte_length);
        assert!(buffer.name.is_none());
        assert!(buffer.uri.is_none());
        let buffer = buffer.uri("test_uri");
        assert_eq!("test_uri", buffer.uri.unwrap());
    }
}
