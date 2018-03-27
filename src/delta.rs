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
    // TODO: from
    // pub fn new(top: UVec) -> Delta {
    //     Delta {
    //         zdelta: 0,
    //         coords: TypedVector2D::new(BigInt::from(top.x), BigInt::from(top.y)),
    //     }
    // }

    pub fn zero() -> Delta {
        Delta {
            zdelta: 0,
            coords: TypedVector2D::new(BigInt::zero(), BigInt::zero()),
        }
    }

    fn add_vec<T: Integer, U>(v: TypedVector2D<T, U>, u: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        TypedVector2D::new(v.x + u.x, v.y + u.y)
    }

    fn div_vec<T: Integer + From<u32>, U>(mut v: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        v.x = v.x.div_floor(&T::from(ZOOM_SCALE));
        v.y = v.y.div_floor(&T::from(ZOOM_SCALE));
        v
    }

    fn mult_vec<T: Integer + From<u32>, U>(mut v: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        v.x = v.x * (T::from(ZOOM_SCALE));
        v.y = v.y * (T::from(ZOOM_SCALE));
        v
    }

    fn pow_vec<T: Integer + From<u32> + Clone, U>(mut v: TypedVector2D<T, U>, p: usize) -> TypedVector2D<T, U> {
        v.x = v.x * pow(T::from(ZOOM_SCALE), p);
        v.y = v.y * pow(T::from(ZOOM_SCALE), p);
        v
    }



    pub fn shift_target_down(mut self) -> Delta {
        self.coords.x = self.coords.x * (BigInt::from(ZOOM_SCALE));
        self.coords.y = self.coords.y * (BigInt::from(ZOOM_SCALE));

        self.zdelta += 1;
        self
    }

    pub fn shift_target_down_ref(&self) -> Delta {
        self.clone().shift_target_down()
    }

    pub fn truncate_target_up(&mut self) -> () {
        self.coords.x = self.coords.x.div_floor(&BigInt::from(ZOOM_SCALE));
        self.coords.y = self.coords.y.div_floor(&BigInt::from(ZOOM_SCALE));

        self.zdelta -= 1;
    }

    pub fn to_uvec(&self) -> UVec {
        UVec::new(
            self.coords.x.to_i32().unwrap(),
            self.coords.y.to_i32().unwrap(),
        )
    }

    // This composes in diagrammatic order
    // Only makes sense when both zdeltas are positive.
    pub fn append(self, other: &Delta) -> Delta {
        let zdelta = self.zdelta + other.zdelta;
        let coords = Delta::add_vec(Delta::pow_vec(self.coords, other.zdelta as usize), other.coords.clone());

        Delta { zdelta, coords }
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
            zdelta: 0,
            coords: TypedVector2D::new(BigInt::from(c.x), BigInt::from(c.y)),
        }
    }
}
