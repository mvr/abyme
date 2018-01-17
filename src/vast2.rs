use std::collections::VecDeque;
use std::ops::Add;

use types::*;

pub struct Vast2 {
    pub zdelta: u16,
    pub coord_offset: u16,
    pub coords: VecDeque<IVec2>
}

impl Vast2 {
    pub fn new(IVec2) {
    }

    fn normalize(&mut self) {

    }
}

impl PartialEq for Vast2<Other = Vast2> {

}

impl Eq for Vast2 {}

impl Add for Vast2 {
    type Output = Vast2;

    fn add(self, other: Vast2) -> Vast2 {
        unimplemented!();
    }
}
