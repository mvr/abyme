use std::collections::VecDeque;
use std::ops::Add;

use types::*;

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

    fn normalize(&mut self) {

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
