//! GLTF writer.

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Gltf {
    pub asset: Asset,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub nodes: Vec<Node>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub scenes: Vec<Scene>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub buffers: Vec<Buffer>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub scene: Option<u32>
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

    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub buffers: Vec<Buffer>
}

#[derive(Serialize, Deserialize)]
pub struct Node {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<u32>,
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

#[derive(Serialize, Deserialize)]
pub struct Accessor {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(rename(serialize = "componentType"))]
    pub component_type: u32,

    pub count: u32,

    #[serde(rename(serialize = "type"))]
    pub attribute_type: String,

    #[serde(rename(serialize = "bufferView"))]
    pub buffer_view_index: u32,

    #[serde(rename(serialize = "byteOffset"))]
    #[serde(skip_serializing_if = "is_zero")]
    pub byte_offset: u32, // optional, but defaults to zero

    #[serde(skip_serializing_if = "is_false")]
    pub normalized: bool, // optional, but defaults to false
}

impl Gltf {
    pub fn new(asset: Asset) -> Gltf {
        Gltf {
            asset,
            nodes: vec![],
            scenes: vec![],
            buffers: vec![],
            scene: None
        }
    }
}

impl Asset {
    pub fn new(version: &str) -> Asset {
        Asset {
            version: version.to_string(),
            min_version: None,
            generator: None,
            copyright: None
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
        assert_eq!(serde_json::to_string(&gltf).unwrap(), "{\"asset\":{\"version\":\"2.0\"}}".to_string());
    }

    #[test]
    fn asset() {
        let asset = Asset::new("2.0");
        assert_eq!(serde_json::to_string(&asset).unwrap(), "{\"version\":\"2.0\"}".to_string());
        let asset = Asset::new("2.0").generator("weldr");
        assert_eq!(serde_json::to_string(&asset).unwrap(), "{\"version\":\"2.0\",\"generator\":\"weldr\"}".to_string());
        let asset = Asset::new("2.0").min_version("1.0");
        assert_eq!(serde_json::to_string(&asset).unwrap(), "{\"version\":\"2.0\",\"minVersion\":\"1.0\"}".to_string());
        let asset = Asset::new("2.0").copyright("(c) weldr");
        assert_eq!(serde_json::to_string(&asset).unwrap(), "{\"version\":\"2.0\",\"copyright\":\"(c) weldr\"}".to_string());
    }

}