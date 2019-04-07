use rug::ops::Pow;
use rug::Integer;
use std::ops::Div;
// use std::ops::{Add, Neg, Sub};

use euclid::*;

use defs::*;
use math;

// A "vector" in the world, possibly going up or down levels
// TODO: Do we need a `Dyadic`?

#[derive(Clone, Debug, PartialEq, Eq)] // MUST TODO: this Eq is dangerous!
pub struct Delta {
    pub zdelta: i16,
    pub scale: i16,
    pub coords: TypedVector2D<Integer, UniverseSpace>,
    // So a coord distance of 1 means a `real` distance of 2^(scale) at z value 0
}

impl Delta {
    pub fn zero() -> Delta {
        Delta {
            zdelta: 0,
            scale: 0,
            coords: TypedVector2D::new(Integer::new(), Integer::new()),
        }
    }

    pub fn append(&self, other: &Delta) -> Delta {
        let zdelta = self.zdelta + other.zdelta;
        let other_relative_scale = self.zdelta + other.scale;
        let newscale = self.scale.min(other_relative_scale);

        let newcoords: TypedVector2D<Integer, UniverseSpace>;
        if self.scale == other_relative_scale {
            newcoords = math::add_vec(self.coords.clone(), other.coords.clone());
        } else if self.scale >= other_relative_scale {
            let difference = (self.scale - other_relative_scale) as u32;
            newcoords = math::add_vec(
                math::scale_vec(&self.coords, &Integer::from(ZOOM_SCALE).pow(difference)),
                other.coords.clone(),
            );
        } else {
            let difference = (other_relative_scale - self.scale) as u32;
            newcoords = math::add_vec(
                math::scale_vec(&other.coords, &Integer::from(ZOOM_SCALE).pow(difference)),
                self.coords.clone(),
            );
        }

        Delta {
            zdelta: zdelta,
            scale: newscale,
            coords: newcoords,
        }
    }

    pub fn invert(&self) -> Delta {
        Delta {
            zdelta: -self.zdelta,
            scale: self.scale - self.zdelta,
            coords: TypedVector2D::new(-self.coords.x.clone(), -self.coords.y.clone()),
        }
    }

    pub fn revert(&self, other: &Delta) -> Delta {
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

    pub fn normalize(&self) -> Delta {
        if self.coords.x == 0 && self.coords.y == 0 {
            return Delta {
                coords: TypedVector2D::new(Integer::new(), Integer::new()),
                scale: 0,
                zdelta: self.zdelta,
            };
        }

        let mut result = self.clone();

        while result.coords.x.is_divisible_2pow(1) && result.coords.x.is_divisible_2pow(1) {
            result.coords.x = result.coords.x.div(2);
            result.coords.y = result.coords.y.div(2);
            result.scale += 1;
        }

        return result;
    }

    pub fn to_uvec(&self) -> Option<UVec> {
        let normalized = self.normalize();

        assert!(normalized.zdelta == 0);
        assert!(normalized.scale >= 0);

        Some(
            UVec::new(
                normalized.coords.x.to_i32().unwrap(),
                normalized.coords.y.to_i32().unwrap(),
            ) * ZOOM_SCALE.pow(normalized.scale as u32) as i32,
        )
    }
}

impl<U> From<TypedVector2D<f32, U>> for Delta
where
    U: SpaceWithLevel,
{
    fn from(c: TypedVector2D<f32, U>) -> Delta {
        let int_x = (c.x * (ZOOM_SCALE as f32).pow(FRACTIONAL_DELTA_SCALE as i32)) as i32;
        let int_y = (c.y * (ZOOM_SCALE as f32).pow(FRACTIONAL_DELTA_SCALE as i32)) as i32;

        Delta {
            zdelta: U::LEVEL,
            scale: U::LEVEL - FRACTIONAL_DELTA_SCALE as i16,
            coords: TypedVector2D::new(Integer::from(int_x), Integer::from(int_y)),
        }
    }
}

impl<U> From<TypedVector2D<i32, U>> for Delta
where
    U: SpaceWithLevel,
{
    fn from(c: TypedVector2D<i32, U>) -> Delta {
        Delta {
            zdelta: U::LEVEL,
            scale: U::LEVEL,
            coords: TypedVector2D::new(Integer::from(c.x), Integer::from(c.y)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fractional_append_1() {
        let a = Delta {
            zdelta: 2,
            scale: 1,
            coords: TypedVector2D::new(Integer::from(-1), Integer::from(0)),
        };
        let b = Delta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(1), Integer::from(0)),
        };

        assert_eq!(
            a.append(&b),
            Delta {
                zdelta: 1,
                scale: 1,
                coords: TypedVector2D::new(Integer::from(0), Integer::from(0)),
            }
        );
    }

    #[test]
    fn fractional_append_2() {
        let a = Delta {
            zdelta: 1,
            scale: 0,
            coords: TypedVector2D::new(Integer::from(0), Integer::from(0)),
        };

        let b = Delta {
            zdelta: 1,
            scale: 1,
            coords: TypedVector2D::new(Integer::from(-1), Integer::from(0)),
        };
        assert_eq!(
            a.append(&b),
            Delta {
                zdelta: 2,
                scale: 0,
                coords: TypedVector2D::new(Integer::from(-4), Integer::from(0)),
            }
        );
    }

    #[test]
    fn fractional_append_3() {
        let a = Delta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(1), Integer::from(0)),
        };

        let b = Delta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(0), Integer::from(0)),
        };
        assert_eq!(
            a.append(&b),
            Delta {
                zdelta: -2,
                scale: -2,
                coords: TypedVector2D::new(Integer::from(2), Integer::from(0)),
            }
        );
    }

}
