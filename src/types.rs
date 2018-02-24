// extern crate euclid;

use num::Integer;

use euclid::*;

pub struct UniverseSpace;
pub struct ChildSpace;

pub type UVec = TypedVector2D<i32, UniverseSpace>;
pub type UPoint = TypedPoint2D<i32, UniverseSpace>;
pub type ChildVec = TypedVector2D<i32, ChildSpace>;
pub type ChildPoint = TypedPoint2D<i32, ChildSpace>;

// pub type FVec = TypedVector<f32>;

pub const zoom_scale: u32 = 2;

pub mod vectors {
    use types::*;

    #[inline]
    pub fn coerce_up(v: ChildVec) -> UVec {
        UVec::new(v.x, v.y)
    }

    #[inline]
    pub fn truncate_up(v: ChildVec) -> UVec {
        UVec::new(
            v.x.div_floor(&(zoom_scale as i32)),
            v.y.div_floor(&(zoom_scale as i32)),
        )
    }

    #[inline]
    pub fn mod_up(v: ChildVec) -> ChildVec {
        ChildVec::new(v.x % zoom_scale as i32, v.y % zoom_scale as i32)
    }

    #[inline]
    pub fn split_up(v: ChildVec) -> (UVec, ChildVec) {
        (truncate_up(v), mod_up(v))
    }

    #[inline]
    pub fn shift_down(v: UVec) -> ChildVec {
        ChildVec::new(v.x * (zoom_scale as i32), v.y * (zoom_scale as i32))
    }

    #[inline]
    pub fn coerce_down(v: UVec) -> ChildVec {
        ChildVec::new(v.x, v.y)
    }
}
