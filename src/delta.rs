use std::collections::VecDeque;
use std::ops::Add;
use cgmath::Vector2;

use types::*;

#[derive(Clone)]
pub struct Delta {
    pub zdelta: u16,

    pub coord_offset: u16,
    pub coords: VecDeque<IVec2>
}

impl Delta {
    pub fn new(top: IVec2) -> Delta {
        Delta {
            zdelta: 0,
            coord_offset: 0,
            coords: VecDeque::from(vec!(top))
        }
    }

    pub fn zero() -> Delta {
        Delta::new(Vector2::new(0, 0))
    }

    fn normalize(&mut self) {
        unimplemented!();
    }

    pub fn shift_down(&self) -> Delta {
        unimplemented!();
    }

    pub fn truncate_up(&self) -> Delta {
        unimplemented!();
    }

    pub fn to_vec2(&self) -> IVec2 {
        // TODO: Detect overflow
        unimplemented!();
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

impl<'a, 'b> Add<&'b Delta> for &'a Delta {
    type Output = Delta;

    fn add(self, other: &'b Delta) -> Delta {
        unimplemented!();
    }
}
