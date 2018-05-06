use std::ops::{Add, Neg, Sub};
use rug::{Integer};
use rug::ops::{DivRounding, Pow};

use euclid::*;

use defs::*;
use math::*;

// A "vector" in the world, possibly going up or down levels

// TODO: If memory use becomes a problem, it may be worth converting this to
// using a persistent data structure and sharing the coords deque as
// much as possible

#[derive(Clone)]
pub struct Delta {
    pub zdelta: i16, // How far DOWN the target is

    // TODO: maybe find a faster bigint
    pub coords: TypedVector2D<Integer, UniverseSpace>,
}

impl Delta {
    pub fn zero() -> Delta {
        Delta {
            zdelta: 0,
            coords: TypedVector2D::new(Integer::new(), Integer::new()),
        }
    }

    fn add_vec<T: Add<T, Output = T>, U>(
        v: TypedVector2D<T, U>,
        u: TypedVector2D<T, U>,
    ) -> TypedVector2D<T, U> {
        TypedVector2D::new(v.x + u.x, v.y + u.y)
    }

    fn sub_vec<T: Sub<T, Output = T>, U>(
        v: TypedVector2D<T, U>,
        u: TypedVector2D<T, U>,
    ) -> TypedVector2D<T, U> {
        TypedVector2D::new(v.x - u.x, v.y - u.y)
    }

    fn scale_vec<U>(v: &TypedVector2D<Integer, U>, amount: Integer) -> TypedVector2D<Integer, U> {
        TypedVector2D::new(amount.clone() * &v.x, amount.clone() * &v.y)
    }

    fn div_vec<U>(v: &TypedVector2D<Integer, U>, amount: &Integer) -> TypedVector2D<Integer, U> {
        TypedVector2D::new(v.x.clone().div_floor(amount), v.y.clone().div_floor(amount))
    }

    pub fn shift_target_down(&self) -> Delta {
        Delta {
            zdelta: self.zdelta + 1,
            coords: Delta::scale_vec(&self.coords, Integer::from(ZOOM_SCALE))
        }
    }

    pub fn shift_target_down_ref(&mut self) -> () {
        self.coords = Delta::scale_vec(&self.coords, Integer::from(ZOOM_SCALE));

        self.zdelta += 1;
    }

    pub fn truncate_target_up(&mut self) -> () {
        self.coords = Delta::div_vec(&self.coords, &Integer::from(ZOOM_SCALE));

        self.zdelta -= 1;
    }

    pub fn to_uvec(&self) -> UVec {
        UVec::new(
            self.coords.x.to_i32().unwrap(),
            self.coords.y.to_i32().unwrap(),
        )
    }

    // TODO: test
    pub fn to_scaled_fvec(&self) -> TypedVector2D<f32, UniverseSpace> {
        TypedVector2D::new(math::scaled_bigint_to_float(&self.coords.x, self.zdelta),
                           math::scaled_bigint_to_float(&self.coords.y, self.zdelta))
    }

    pub fn to_scale_transform(&self) -> TypedTransform2D<f32, UniverseSpace, UniverseSpace> {
        let scale = (ZOOM_SCALE as f32).powi(-self.zdelta as i32);
        TypedTransform2D::identity().post_translate(self.to_scaled_fvec()).post_scale(scale, scale)
    }

    pub fn invert(self) -> Delta {
        Delta {
            zdelta: -self.zdelta,
            coords: TypedVector2D::new(-self.coords.x, -self.coords.y),
        }
    }

    // This composes in diagrammatic order
    // Only makes sense when both zdeltas are positive.
    pub fn append(&self, other: &Delta) -> Delta {
        debug_assert!(other.zdelta >= 0);

        let zdelta = self.zdelta + other.zdelta;
        let coords = Delta::add_vec(
            Delta::scale_vec(
                &self.coords,
                Integer::from(ZOOM_SCALE).pow(other.zdelta as u32),
            ),
            other.coords.clone(),
        );

        Delta { zdelta, coords }
    }

    // This is NOT the same as appending an inverted other
    pub fn revert(&self, other: &Delta) -> Delta {
        debug_assert!(other.zdelta >= 0);

        let zdelta = self.zdelta - other.zdelta;
        let coords = Delta::div_vec(
            &Delta::sub_vec(self.coords.clone(), other.coords.clone()),
            &Integer::from(ZOOM_SCALE).pow(other.zdelta as u32),
        );

        Delta { zdelta, coords }
    }

    // Do maximal movement at level 0, then a residual delta
    pub fn factor(&self) -> (UVec, Delta) {
        unimplemented!();
    }
}

impl PartialEq for Delta {
    fn eq(&self, other: &Delta) -> bool {
        self.zdelta == other.zdelta && self.coords == other.coords
    }
}

impl Eq for Delta {}

impl Add<Delta> for Delta {
    type Output = Delta;

    fn add(self, other: Delta) -> Delta {
        assert!(self.zdelta == other.zdelta);

        Delta {
            zdelta: self.zdelta,
            coords: TypedVector2D::new(
                self.coords.x + other.coords.x,
                self.coords.y + other.coords.y,
            ),
        }
    }
}

impl<'a> Add<&'a Delta> for &'a Delta {
    type Output = Delta;

    fn add(self, other: &Delta) -> Delta {
        assert!(self.zdelta == other.zdelta);

        Delta {
            zdelta: self.zdelta,
            coords: TypedVector2D::new(
                self.coords.x.clone() + other.coords.x.clone(),
                self.coords.y.clone() + other.coords.y.clone(),
            ),
        }
    }
}

impl Sub<Delta> for Delta {
    type Output = Delta;

    fn sub(self, other: Delta) -> Delta {
        assert!(self.zdelta == other.zdelta);

        Delta {
            zdelta: self.zdelta,
            coords: TypedVector2D::new(
                self.coords.x - other.coords.x,
                self.coords.y - other.coords.y,
            ),
        }
    }
}

impl<'a> Sub<&'a Delta> for &'a Delta {
    type Output = Delta;

    fn sub(self, other: &Delta) -> Delta {
        assert!(self.zdelta == other.zdelta);

        Delta {
            zdelta: self.zdelta,
            coords: TypedVector2D::new(
                self.coords.x.clone() - other.coords.x.clone(),
                self.coords.y.clone() - other.coords.y.clone(),
            ),
        }
    }
}

impl Neg for Delta {
    type Output = Delta;

    fn neg(mut self) -> Delta {
        self.coords.x = -self.coords.x;
        self.coords.y = -self.coords.y;
        self
    }
}

impl From<ChildVec> for Delta {
    fn from(c: ChildVec) -> Delta {
        Delta {
            zdelta: 1,
            coords: TypedVector2D::new(Integer::from(c.x), Integer::from(c.y)),
        }
    }
}

impl From<UVec> for Delta {
    fn from(c: UVec) -> Delta {
        Delta {
            zdelta: 0,
            coords: TypedVector2D::new(Integer::from(c.x), Integer::from(c.y)),
        }
    }
}
