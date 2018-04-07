use std::ops::{Add, Neg, Sub};
use num::bigint::BigInt;
use num::pow::pow;
use num::{Integer, ToPrimitive, Zero};

use euclid::*;

use gameplay_constants::*;
use types::*;

// A "vector" in the world, possibly going up or down levels

// TODO: If memory use becomes a problem, it may be worth converting this to
// using a persistent data structure and sharing the coords deque as
// much as possible

#[derive(Clone)]
pub struct Delta {
    pub zdelta: i16, // How far DOWN the target is

    // TODO: maybe find a faster bigint
    pub coords: TypedVector2D<BigInt, UniverseSpace>,
}

impl Delta {
    pub fn zero() -> Delta {
        Delta {
            zdelta: 0,
            coords: TypedVector2D::new(BigInt::zero(), BigInt::zero()),
        }
    }

    fn add_vec<T: Integer, U>(
        v: TypedVector2D<T, U>,
        u: TypedVector2D<T, U>,
    ) -> TypedVector2D<T, U> {
        TypedVector2D::new(v.x + u.x, v.y + u.y)
    }

    fn sub_vec<T: Integer, U>(
        v: TypedVector2D<T, U>,
        u: TypedVector2D<T, U>,
    ) -> TypedVector2D<T, U> {
        TypedVector2D::new(v.x - u.x, v.y - u.y)
    }

    fn scale_vec<T: Integer + Clone, U>(v: &TypedVector2D<T, U>, amount: T) -> TypedVector2D<T, U> {
        TypedVector2D::new(amount.clone() * v.x.clone(), amount.clone() * v.y.clone())
    }

    fn div_vec<T: Integer, U>(v: &TypedVector2D<T, U>, amount: T) -> TypedVector2D<T, U> {
        TypedVector2D::new(v.x.div_floor(&amount), v.y.div_floor(&amount))
    }

    pub fn shift_target_down(&self) -> Delta {
        Delta {
            zdelta: self.zdelta + 1,
            coords: Delta::scale_vec(&self.coords, BigInt::from(ZOOM_SCALE))
        }
    }

    pub fn shift_target_down_ref(&mut self) -> () {
        self.coords = Delta::scale_vec(&self.coords, BigInt::from(ZOOM_SCALE));

        self.zdelta += 1;
    }

    pub fn truncate_target_up(&mut self) -> () {
        self.coords = Delta::div_vec(&self.coords, BigInt::from(ZOOM_SCALE));

        self.zdelta -= 1;
    }

    pub fn to_uvec(&self) -> UVec {
        UVec::new(
            self.coords.x.to_i32().unwrap(),
            self.coords.y.to_i32().unwrap(),
        )
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
                pow(BigInt::from(ZOOM_SCALE), other.zdelta as usize),
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
            pow(BigInt::from(ZOOM_SCALE), other.zdelta as usize),
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
            coords: TypedVector2D::new(BigInt::from(c.x), BigInt::from(c.y)),
        }
    }
}

impl From<UVec> for Delta {
    fn from(c: UVec) -> Delta {
        Delta {
            zdelta: 0,
            coords: TypedVector2D::new(BigInt::from(c.x), BigInt::from(c.y)),
        }
    }
}
