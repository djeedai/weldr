#![cfg(not(feature = "cgmath"))]

/// Generic 3-component vector for drawing commands.
///
/// With the `cgmath` feature (default), this is an alias to `cgmath::Vector3<f32>`.
/// Without it, the `Vec3` type is defined as a built-in struct with basic operations overloaded.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3 {
    pub fn dot(&self, other: &Vec3) -> f32 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    pub fn normalized(&self) -> Vec3 {
        let invsqrt: f32 = self.dot(self).sqrt().recip();
        self * invsqrt
    }
}

impl std::ops::Add<Vec3> for Vec3 {
    type Output = Vec3;

    fn add(self, other: Vec3) -> Vec3 {
        Vec3 {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl std::ops::Add<&Vec3> for Vec3 {
    type Output = Vec3;

    fn add(self, other: &Vec3) -> Vec3 {
        Vec3 {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl<'a> std::ops::Add<&Vec3> for &'a Vec3 {
    type Output = Vec3;

    fn add(self, other: &Vec3) -> Vec3 {
        Vec3 {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl<'a> std::ops::Add<Vec3> for &'a Vec3 {
    type Output = Vec3;

    fn add(self, other: Vec3) -> Vec3 {
        Vec3 {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl std::ops::Add<f32> for Vec3 {
    type Output = Vec3;

    fn add(self, other: f32) -> Vec3 {
        Vec3 {
            x: self.x + other,
            y: self.y + other,
            z: self.z + other,
        }
    }
}

impl<'a> std::ops::Add<f32> for &'a Vec3 {
    type Output = Vec3;

    fn add(self, other: f32) -> Vec3 {
        Vec3 {
            x: self.x + other,
            y: self.y + other,
            z: self.z + other,
        }
    }
}

impl std::ops::AddAssign<Vec3> for Vec3 {
    fn add_assign(&mut self, other: Vec3) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

impl std::ops::AddAssign<&Vec3> for Vec3 {
    fn add_assign(&mut self, other: &Vec3) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

impl<'a> std::ops::Sub<&Vec3> for &'a Vec3 {
    type Output = Vec3;

    fn sub(self, other: &Vec3) -> Vec3 {
        Vec3 {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl<'a> std::ops::Mul<&Vec3> for &'a Vec3 {
    type Output = Vec3;

    fn mul(self, other: &Vec3) -> Vec3 {
        Vec3 {
            x: self.x * other.x,
            y: self.y * other.y,
            z: self.z * other.z,
        }
    }
}

impl std::ops::Mul<f32> for Vec3 {
    type Output = Vec3;

    fn mul(self, other: f32) -> Vec3 {
        Vec3 {
            x: self.x * other,
            y: self.y * other,
            z: self.z * other,
        }
    }
}

impl<'a> std::ops::Mul<f32> for &'a Vec3 {
    type Output = Vec3;

    fn mul(self, other: f32) -> Vec3 {
        Vec3 {
            x: self.x * other,
            y: self.y * other,
            z: self.z * other,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_ctor() {
        let v = Vec3 {
            x: 1.2,
            y: -3.6,
            z: 34.67,
        };
        assert_eq!(1.2, v.x);
        assert_eq!(-3.6, v.y);
        assert_eq!(34.67, v.z);
    }

    #[test]
    fn test_dot() {
        let v1 = Vec3 {
            x: 1.0,
            y: 2.0,
            z: -3.0,
        };
        let v2 = Vec3 {
            x: 3.0,
            y: 2.0,
            z: 1.0,
        };
        assert_eq!(4.0, v1.dot(&v2));
    }

    #[test]
    fn test_normalized() {
        let v = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        let r = 1.0 / 3.0_f32.sqrt();
        assert_eq!(Vec3 { x: r, y: r, z: r }, v.normalized());
    }

    #[test]
    fn test_add() {
        let v1 = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        let v2 = Vec3 {
            x: 2.0,
            y: 0.0,
            z: -1.0,
        };
        let res = Vec3 {
            x: 3.0,
            y: 1.0,
            z: 0.0,
        };
        assert_eq!(res, v1 + v2);
        assert_eq!(res, v1 + &v2);
        assert_eq!(res, &v1 + v2);
        assert_eq!(res, &v1 + &v2);
    }

    #[test]
    fn test_add_scalar() {
        let v = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        let f = 3.0_f32;
        let res = Vec3 {
            x: 4.0,
            y: 4.0,
            z: 4.0,
        };
        assert_eq!(res, v + f);
        assert_eq!(res, &v + f);
    }

    #[test]
    fn test_add_assign() {
        let res = Vec3 {
            x: 3.0,
            y: 1.0,
            z: 0.0,
        };
        let v2 = Vec3 {
            x: 2.0,
            y: 0.0,
            z: -1.0,
        };
        let mut v1 = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        v1 += v2;
        assert_eq!(res, v1);
        let mut v1 = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        v1 += &v2;
        assert_eq!(res, v1);
    }

    #[test]
    fn test_sub() {
        let v1 = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        let v2 = Vec3 {
            x: 2.0,
            y: 0.0,
            z: -1.0,
        };
        let res = Vec3 {
            x: -1.0,
            y: 1.0,
            z: 2.0,
        };
        assert_eq!(res, &v1 - &v2);
    }

    #[test]
    fn test_mul() {
        let v1 = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        let v2 = Vec3 {
            x: 2.0,
            y: 0.0,
            z: -1.0,
        };
        let res = Vec3 {
            x: 2.0,
            y: 0.0,
            z: -1.0,
        };
        assert_eq!(res, &v1 * &v2);
    }

    #[test]
    fn test_mul_scalar() {
        let v = Vec3 {
            x: 1.0,
            y: 1.0,
            z: 1.0,
        };
        let f = 3.0_f32;
        let res = Vec3 {
            x: 3.0,
            y: 3.0,
            z: 3.0,
        };
        assert_eq!(res, v * f);
        assert_eq!(res, &v * f);
    }
}
