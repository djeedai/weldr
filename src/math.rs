
/// Generic 3-component vector.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32
}

impl Vec3 {
    pub fn dot(&self, other: &Vec3) -> f32 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    pub fn normalized(&self) -> Vec3 {
        let invsqrt : f32 = self.dot(self).sqrt().recip();
        self * invsqrt 
    }
}

impl std::ops::Add<Vec3> for Vec3 {
  type Output = Vec3;

  fn add(self, other: Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl std::ops::Add<&Vec3> for Vec3 {
  type Output = Vec3;

  fn add(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl<'a> std::ops::Add<&Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn add(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl<'a> std::ops::Add<Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn add(self, other: Vec3) -> Vec3 {
      Vec3 {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z
      }
  }
}

impl std::ops::Add<f32> for Vec3 {
  type Output = Vec3;

  fn add(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x + other,
          y: self.y + other,
          z: self.z + other
      }
  }
}

impl<'a> std::ops::Add<f32> for &'a Vec3 {
  type Output = Vec3;

  fn add(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x + other,
          y: self.y + other,
          z: self.z + other
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
          z: self.z - other.z
      }
  }
}

impl<'a> std::ops::Mul<&Vec3> for &'a Vec3 {
  type Output = Vec3;

  fn mul(self, other: &Vec3) -> Vec3 {
      Vec3 {
          x: self.x * other.x,
          y: self.y * other.y,
          z: self.z * other.z
      }
  }
}

impl std::ops::Mul<f32> for Vec3 {
  type Output = Vec3;

  fn mul(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x * other,
          y: self.y * other,
          z: self.z * other
      }
  }
}

impl<'a> std::ops::Mul<f32> for &'a Vec3 {
  type Output = Vec3;

  fn mul(self, other: f32) -> Vec3 {
      Vec3 {
          x: self.x * other,
          y: self.y * other,
          z: self.z * other
      }
  }
}
