use std::ops::{Add, Neg, Sub};
use rug::Integer;
use rug::ops::{DivRounding, Pow};

use euclid::*;

use defs::*;
use math;

// A "vector" in the world, possibly going up or down levels

// TODO: If memory use becomes a problem, it may be worth converting this to
// using a persistent data structure and sharing the coords deque as
// much as possible

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Delta {
    pub zdelta: i16, // How far UP the target is
    pub coords: TypedVector2D<Integer, UniverseSpace>,
}

impl Delta {
    pub fn zero() -> Delta {
        Delta {
            zdelta: 0,
            coords: TypedVector2D::new(Integer::new(), Integer::new()),
        }
    }

    pub fn shift_target_down(&self) -> Delta {
        Delta {
            zdelta: self.zdelta - 1,
            coords: math::scale_vec(&self.coords, Integer::from(ZOOM_SCALE)),
        }
    }

    pub fn shift_target_down_ref(&mut self) -> () {
        self.coords = math::scale_vec(&self.coords, Integer::from(ZOOM_SCALE));

        self.zdelta -= 1;
    }

    pub fn truncate_target_up(&mut self) -> () {
        self.coords = math::div_vec(&self.coords, &Integer::from(ZOOM_SCALE));

        self.zdelta += 1;
    }

    pub fn to_uvec(&self) -> UVec {
        UVec::new(
            self.coords.x.to_i32().unwrap(),
            self.coords.y.to_i32().unwrap(),
        )
    }

    // TODO: test
    pub fn to_scaled_fvec(&self) -> TypedVector2D<f32, UniverseSpace> {
        TypedVector2D::new(
            math::scaled_bigint_to_float(&self.coords.x, self.zdelta),
            math::scaled_bigint_to_float(&self.coords.y, self.zdelta),
        )
    }

    pub fn to_scale_transform(&self) -> TypedTransform2D<f32, UniverseSpace, UniverseSpace> {
        let scale = (ZOOM_SCALE as f32).powi(self.zdelta as i32);
        TypedTransform2D::identity()
            .post_scale(scale, scale)
            .post_translate(self.to_scaled_fvec())
    }

    // pub fn invert(self) -> Delta {
    //     Delta {
    //         zdelta: -self.zdelta,
    //         coords: TypedVector2D::new(-self.coords.x, -self.coords.y),
    //     }
    // }

    // This composes in diagrammatic order
    // Only makes sense when both zdeltas are negative.
    pub fn append(&self, other: &Delta) -> Delta {
        debug_assert!(other.zdelta <= 0);

        let zdelta = self.zdelta + other.zdelta;
        let coords = math::add_vec(
            math::scale_vec(
                &self.coords,
                Integer::from(ZOOM_SCALE).pow(-other.zdelta as u32),
            ),
            other.coords.clone(),
        );

        Delta { zdelta, coords }
    }

    // This is NOT the same as appending an inverted other
    pub fn revert(&self, other: &Delta) -> Delta {
        debug_assert!(other.zdelta <= 0);

        let zdelta = self.zdelta - other.zdelta;
        let coords = math::div_vec(
            &math::sub_vec(self.coords.clone(), other.coords.clone()),
            &Integer::from(ZOOM_SCALE).pow(-other.zdelta as u32),
        );

        Delta { zdelta, coords }
    }

    // // Do maximal movement at level 0, then a residual delta
    // pub fn factor(&self) -> (UVec, Delta) {
    //     unimplemented!();
    // }
}

// impl Add<Delta> for Delta {
//     type Output = Delta;

//     fn add(self, other: Delta) -> Delta {
//         assert!(self.zdelta == other.zdelta);

//         Delta {
//             zdelta: self.zdelta,
//             coords: TypedVector2D::new(
//                 self.coords.x + other.coords.x,
//                 self.coords.y + other.coords.y,
//             ),
//         }
//     }
// }

// impl<'a> Add<&'a Delta> for &'a Delta {
//     type Output = Delta;

//     fn add(self, other: &Delta) -> Delta {
//         assert!(self.zdelta == other.zdelta);

//         Delta {
//             zdelta: self.zdelta,
//             coords: TypedVector2D::new(
//                 self.coords.x.clone() + other.coords.x.clone(),
//                 self.coords.y.clone() + other.coords.y.clone(),
//             ),
//         }
//     }
// }

// impl Sub<Delta> for Delta {
//     type Output = Delta;

//     fn sub(self, other: Delta) -> Delta {
//         assert!(self.zdelta == other.zdelta);

//         Delta {
//             zdelta: self.zdelta,
//             coords: TypedVector2D::new(
//                 self.coords.x - other.coords.x,
//                 self.coords.y - other.coords.y,
//             ),
//         }
//     }
// }

// impl<'a> Sub<&'a Delta> for &'a Delta {
//     type Output = Delta;

//     fn sub(self, other: &Delta) -> Delta {
//         assert!(self.zdelta == other.zdelta);

//         Delta {
//             zdelta: self.zdelta,
//             coords: TypedVector2D::new(
//                 self.coords.x.clone() - other.coords.x.clone(),
//                 self.coords.y.clone() - other.coords.y.clone(),
//             ),
//         }
//     }
// }

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
            zdelta: -1,
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

// Do we need a `Dyadic`?

#[derive(Clone, Debug, PartialEq, Eq)] // MUST TODO: this Eq is dangerous!
pub struct FractionalDelta {
    pub zdelta: i16,
    pub scale: i16,
    pub coords: TypedVector2D<Integer, UniverseSpace>,
    // So a coord distance of 1 means a `real` distance of 2^(scale) at z value 0
}

impl FractionalDelta {
    pub fn zero() -> FractionalDelta {
        FractionalDelta {
            zdelta: 0,
            scale: 0,
            coords: TypedVector2D::new(Integer::new(), Integer::new()),
        }
    }

    pub fn append(&self, other: &FractionalDelta) -> FractionalDelta {
        let zdelta = self.zdelta + other.zdelta;
        let other_relative_scale = self.zdelta + other.scale;
        let newscale = self.scale.min(other_relative_scale);

        let newcoords: TypedVector2D<Integer, UniverseSpace>;
        if self.scale == other_relative_scale {
            newcoords = math::add_vec(self.coords.clone(), other.coords.clone());
        } else if self.scale >= other_relative_scale {
            let difference = (self.scale - other_relative_scale) as u32;
            newcoords = math::add_vec(
                math::scale_vec(&self.coords, Integer::from(ZOOM_SCALE).pow(difference)),
                other.coords.clone(),
            );
        } else {
            let difference = (other_relative_scale - self.scale) as u32;
            newcoords = math::add_vec(
                math::scale_vec(&other.coords, Integer::from(ZOOM_SCALE).pow(difference)),
                self.coords.clone(),
            );
        }

        FractionalDelta {
            zdelta: zdelta,
            scale: newscale,
            coords: newcoords,
        }
    }

    pub fn invert(&self) -> FractionalDelta {
        FractionalDelta {
            zdelta: -self.zdelta,
            scale: self.scale - self.zdelta,
            coords: TypedVector2D::new(-self.coords.x.clone(), -self.coords.y.clone()),
        }
    }

    pub fn revert(&self, other: &FractionalDelta) -> FractionalDelta {
        self.append(&other.invert())
    }

    // pub fn truncate(&self) -> Delta {
    //     if self.scale == self.zdelta {
    //         Delta {
    //             zdelta: self.zdelta,
    //             coords: self.coords.clone(),
    //         }
    //     } else if self.scale > self.zdelta {
    //         let difference = (self.scale - self.zdelta) as u32;
    //         let newcoords =
    //             math::scale_vec(&self.coords, Integer::from(ZOOM_SCALE).pow(difference));

    //         Delta {
    //             zdelta: self.zdelta,
    //             coords: newcoords,
    //         }
    //     } else {
    //         let difference = (self.zdelta - self.scale) as u32;
    //         let truncated_coords_x = self.coords.x.clone() >> difference;
    //         let truncated_coords_y = self.coords.y.clone() >> difference;
    //         Delta {
    //             zdelta: self.zdelta,
    //             coords: TypedVector2D::new(truncated_coords_x, truncated_coords_y),
    //         }
    //     }
    // }

    pub fn to_scaled_fvec(&self) -> TypedVector2D<f32, UniverseSpace> {
        TypedVector2D::new(
            math::scaled_bigint_to_float(&self.coords.x, self.scale),
            math::scaled_bigint_to_float(&self.coords.y, self.scale),
        )
    }

    pub fn to_scale_transform(&self) -> TypedTransform2D<f32, UniverseSpace, UniverseSpace> {
        let scale = (ZOOM_SCALE as f32).powi(self.zdelta as i32);
        TypedTransform2D::identity()
            .post_scale(scale, scale)
            .post_translate(self.to_scaled_fvec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fractional_append_1() {
        let a = FractionalDelta {
            zdelta: 2,
            scale: 1,
            coords: TypedVector2D::new(Integer::from(-1), Integer::from(0)),
        };
        let b = FractionalDelta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(1), Integer::from(0)),
        };

        assert_eq!(
            a.append(&b),
            FractionalDelta {
                zdelta: 1,
                scale: 1,
                coords: TypedVector2D::new(Integer::from(0), Integer::from(0)),
            }
        );
    }

    #[test]
    fn fractional_append_2() {
        let a = FractionalDelta {
            zdelta: 1,
            scale: 0,
            coords: TypedVector2D::new(Integer::from(0), Integer::from(0)),
        };

        let b = FractionalDelta {
            zdelta: 1,
            scale: 1,
            coords: TypedVector2D::new(Integer::from(-1), Integer::from(0)),
        };
        assert_eq!(
            a.append(&b),
            FractionalDelta {
                zdelta: 2,
                scale: 1,
                coords: TypedVector2D::new(Integer::from(-1), Integer::from(0)),
            }
        );
    }

    #[test]
    fn fractional_append_3() {
        let a = FractionalDelta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(1), Integer::from(0)),
        };

        let b = FractionalDelta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(0), Integer::from(0)),
        };
        assert_eq!(
            a.append(&b),
            FractionalDelta {
                zdelta: -2,
                scale: -2,
                coords: TypedVector2D::new(Integer::from(2), Integer::from(0)),
            }
        );
    }

}

impl From<TypedVector2D<f32, UniverseSpace>> for FractionalDelta {
    fn from(c: TypedVector2D<f32, UniverseSpace>) -> FractionalDelta {
        let int_x = (c.x / (2.0 as f32).pow(FRACTIONAL_DELTA_SCALE as i32)) as u32;
        let int_y = (c.y / (2.0 as f32).pow(FRACTIONAL_DELTA_SCALE as i32)) as u32;

        FractionalDelta {
            zdelta: 0,
            scale: FRACTIONAL_DELTA_SCALE as i16,
            coords: TypedVector2D::new(Integer::from(int_x), Integer::from(int_y)),
        }
    }
}

impl From<UVec> for FractionalDelta {
    fn from(c: UVec) -> FractionalDelta {
        FractionalDelta {
            zdelta: 0,
            scale: 0,
            coords: TypedVector2D::new(Integer::from(c.x), Integer::from(c.y)),
        }
    }
}

impl From<Delta> for FractionalDelta {
    fn from(c: Delta) -> FractionalDelta {
        FractionalDelta {
            zdelta: c.zdelta,
            scale: c.zdelta,
            coords: c.coords,
        }
    }
}
