extern crate euclid;

use euclid::*;

pub struct UniverseSpace;
pub struct ChildSpace;

pub type UVec = TypedVector2D<i32, UniverseSpace>;
pub type UPoint = TypedPoint2D<i32, UniverseSpace>;
pub type ChildVec = TypedVector2D<i32, ChildSpace>;
pub type ChildPoint = TypedPoint2D<i32, ChildSpace>;

// pub type FVec = TypedVector<f32>;

pub const zoom_scale: u32 = 2;
