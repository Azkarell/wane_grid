use std::ops::{Add, Div, Mul, Sub};

use bevy::{
    ecs::{component::Component, name::Name},
    math::{IVec3, Vec2, Vec3, Vec3Swizzles},
    mesh::Mesh2d,
    reflect::Reflect,
    sprite_render::{ColorMaterial, MeshMaterial2d},
    transform::components::Transform,
};
use serde::{Deserialize, Serialize};

use crate::{AXIAL_CONVERT, AXIAL_INVERTED};

#[derive(
    Component, Debug, PartialEq, Eq, Clone, Copy, Hash, Reflect, Default, Serialize, Deserialize,
)]
#[require(Transform, Mesh2d, MeshMaterial2d<ColorMaterial>, Name=Name::new("GridIndex"))]
pub struct GridIndex {
    pub q: i32,
    pub r: i32,
}

impl From<IVec3> for GridIndex {
    fn from(value: IVec3) -> Self {
        GridIndex {
            q: value.x,
            r: value.y,
        }
    }
}

impl Add for GridIndex {
    type Output = GridIndex;

    fn add(self, rhs: Self) -> Self::Output {
        GridIndex::new(self.q + rhs.q, self.r + rhs.r)
    }
}

impl Sub for GridIndex {
    type Output = GridIndex;

    fn sub(self, rhs: Self) -> Self::Output {
        GridIndex::new(self.q - rhs.q, self.r - rhs.r)
    }
}

impl Mul<i32> for GridIndex {
    type Output = GridIndex;

    fn mul(self, rhs: i32) -> Self::Output {
        GridIndex::new(self.q * rhs, self.r * rhs)
    }
}

impl Div<i32> for GridIndex {
    type Output = GridIndex;

    fn div(self, rhs: i32) -> Self::Output {
        GridIndex::new(self.q / rhs, self.r / rhs)
    }
}

impl GridIndex {
    pub const ZERO: GridIndex = GridIndex::new(0, 0);
    pub const fn new(q: i32, r: i32) -> Self {
        Self { q, r }
    }
    pub fn to_world_pos(&self, size: f32) -> Vec2 {
        let mat = *AXIAL_CONVERT;
        let vec = Vec2::new(self.q as f32, self.r as f32);
        mat.mul_vec2(vec) * size
    }

    pub fn from_row_col(row: i32, col: i32) -> Self {
        let col = col;
        let row = row + (-(col as f32) / 2.0).ceil() as i32;
        Self { q: col, r: row }
    }

    pub fn from_cube_vec(vec: Vec3) -> Self {
        let rounded = cube_round(vec);

        GridIndex {
            q: rounded.x as i32,
            r: rounded.y as i32,
        }
    }

    pub fn from_axial_vec(vec: Vec2) -> Self {
        let rounded = axial_round(vec);

        GridIndex {
            q: rounded.x as i32,
            r: rounded.y as i32,
        }
    }

    pub fn from_world_pos(world_position: Vec2, size: f32) -> Self {
        let x = world_position.x / size;
        let y = world_position.y / size;
        let q = AXIAL_INVERTED.mul_vec2(Vec2::new(x, y));

        GridIndex::from_axial_vec(q)
    }

    pub fn distance(&self, other: &GridIndex) -> u32 {
        axial_distance(self, other)
    }
}

pub fn axial_to_cube(vec: Vec2) -> Vec3 {
    Vec3::new(vec.x, vec.y, -vec.x - vec.y)
}

pub fn cube_to_axial(vec: Vec3) -> Vec2 {
    vec.xy()
}

pub fn axial_round(vec: Vec2) -> Vec2 {
    let v = axial_to_cube(vec);
    let r = cube_round(v);
    cube_to_axial(r)
}

pub fn axial_distance(lhs: &GridIndex, rhs: &GridIndex) -> u32 {
    (((lhs.q - rhs.q).abs() + (lhs.q + lhs.r - rhs.q - rhs.r).abs() + (lhs.r - rhs.r).abs()) / 2)
        as u32
}

pub fn cube_round(vec: Vec3) -> Vec3 {
    let mut rounded = vec.round();
    let diff = (rounded - vec).abs();

    if diff.x > diff.y && diff.x > diff.z {
        rounded.x = -rounded.y - rounded.z;
    } else if diff.y > diff.z {
        rounded.y = -rounded.x - rounded.z;
    } else {
        rounded.z = -rounded.x - rounded.y;
    }
    rounded
}
