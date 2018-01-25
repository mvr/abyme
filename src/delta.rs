use std::collections::VecDeque;
use std::ops::{Add, Neg};
use cgmath::Vector2;

use types::*;

// A "vector" in the world, possibly going up or down levels

// TODO: If memory use becomes a problem, it may be worth converting this to
// using a persistent data structure and sharing the coords deque as
// much as possible

#[derive(Clone)]
pub struct Delta {
    pub zdelta: i16, // How far DOWN the target is

    pub coord_offset: i16, // How far DOWN the coords start
    pub coords: VecDeque<IVec2>,
}

impl Delta {
    pub fn new(top: IVec2) -> Delta {
        Delta {
            zdelta: 0,
            coord_offset: 0,
            coords: VecDeque::from(vec![top]),
        }
    }

    pub fn zero() -> Delta {
        Delta::new(Vector2::new(0, 0))
    }

    fn normalize(&mut self) {
        // Delete 0s from front and end, adjusting coord_offset
        // appropriately

        unimplemented!();
    }

    pub fn shift_target_down(mut self) -> Delta {
        self.coords.push_back(Vector2::new(0, 0));
        self.zdelta += 1;
        self
    }

    pub fn shift_target_down_ref(&self) -> Delta {
        self.clone().shift_target_down()
    }

    pub fn truncate_target_up(mut self) -> Delta {
        unimplemented!();

        // This is wrong: Doesn't mod the last coord
        // self.coords.pop_back();
        // self.zdelta -= 1;
        // self
    }

    pub fn truncate_target_up_ref(&self) -> Delta {
        self.clone().truncate_target_up()
    }

    pub fn to_vec2(&self) -> IVec2 {
        // TODO: Detect overflow

        let mut res = Vector2::new(0, 0);

        for (i, v) in self.coords.iter().enumerate() {
            res += Vector2::new(
                v.x * zoom_scale.pow(i as u32) as i32,
                v.y * zoom_scale.pow(i as u32) as i32,
            );
        }

        if self.coord_offset < 0 {
            let extra_power = (-self.coord_offset) as u32;
            res = Vector2::new(
                res.x * zoom_scale.pow(extra_power) as i32,
                res.y * zoom_scale.pow(extra_power) as i32,
            );
        }

        res
    }
}

impl PartialEq for Delta {
    fn eq(&self, other: &Delta) -> bool {
        unimplemented!();
    }
}

impl Eq for Delta {}

impl Add for Delta {
    type Output = Delta;

    fn add(self, other: Delta) -> Delta {
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
