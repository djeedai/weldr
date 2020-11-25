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
}

fn is_zero(value: &u32) -> bool {
    *value == 0
}

fn is_false(value: &bool) -> bool {
    *value == false
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
}

#[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
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

#[derive(Serialize, Deserialize)]
pub struct Primitive {
    pub attributes: HashMap<String, u32>,

    #[serde(skip_serializing_if = "is_zero")]
    pub indices: u32, // optional, but defaults to zero

    pub mode: PrimitiveMode,
}

#[derive(Serialize, Deserialize)]
pub struct Mesh {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    pub primitives: Vec<Primitive>,
}

impl Gltf {
    pub fn new(asset: Asset) -> Gltf {
        Gltf {
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
        if self.scene == None {
            self.scene = Some(0);
        }
    }
}

impl Asset {
    pub fn new(version: &str) -> Asset {
        Asset {
            version: version.to_string(),
            min_version: None,
            generator: None,
            copyright: None,
        }
    }

    #[inline(always)]
    pub fn min_version(mut self, min_version: &str) -> Self {
        self.min_version = Some(min_version.to_string());
        self
    }

    #[inline(always)]
    pub fn generator(mut self, generator: &str) -> Self {
        self.generator = Some(generator.to_string());
        self
    }

    #[inline(always)]
    pub fn copyright(mut self, copyright: &str) -> Self {
        self.copyright = Some(copyright.to_string());
        self
    }
}

#[cfg(test)]
mod tests {

    use super::*;

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
}
