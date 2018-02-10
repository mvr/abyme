use std::collections::VecDeque;
use std::ops::{Add, Neg};
use num::bigint::BigInt;
use num::Integer;
use num::Zero;

use euclid::*;

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
            coords: TypedVector2D::new(BigInt::zero(), BigInt::zero())
        }
    }

    fn div_vec<T : Integer + From<u32>, U>(v: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        v.x = v.x.div_floor(&T::from(zoom_scale));
        v.y = v.y.div_floor(&T::from(zoom_scale));
        v
    }

    fn mult_vec<T : Integer + From<u32>, U>(v: TypedVector2D<T, U>) -> TypedVector2D<T, U> {
        v.x = v.x * (T::from(zoom_scale));
        v.y = v.y * (T::from(zoom_scale));
        v
    }


    pub fn shift_target_down(mut self) -> Delta {
        self.coords.x = self.coords.x * (BigInt::from(zoom_scale));
        self.coords.y = self.coords.y * (BigInt::from(zoom_scale));

        self.zdelta += 1;
        self
    }

    pub fn shift_target_down_ref(&self) -> Delta {
        self.clone().shift_target_down()
    }


    pub fn truncate_target_up(&mut self) -> () {
        self.coords.x = self.coords.x.div_floor(&BigInt::from(zoom_scale));
        self.coords.y = self.coords.y.div_floor(&BigInt::from(zoom_scale));

        self.zdelta -= 1;
    }

}

impl PartialEq for Delta {
    fn eq(&self, other: &Delta) -> bool {
        unimplemented!();
    }
}

impl Eq for Delta {}

impl<'a> Add<&'a Delta> for &'a Delta {
    type Output = Delta;

    fn add(self, other: &Delta) -> Delta {
        unimplemented!();
    }
}

// impl<'a, 'b> Add<&'b Delta> for &'a Delta {
//     type Output = Delta;

//     fn add(self, other: &'b Delta) -> Delta {
//         unimplemented!();
//     }
// }

impl Neg for Delta {
    type Output = Delta;

    fn neg(mut self) -> Delta {
        self.coords.iter_mut().map(|c| c.neg());
        self
    }
}
