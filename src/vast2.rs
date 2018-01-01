use std::collections::VecDeque;
use std::ops::Add;

use types::*;

pub struct Vast2 {
    pub coords: VecDeque<IVec2>
}

impl Vast2 {
    pub fn new() {
    }
}

impl Add for Vast2 {
    type Output = Vast2;

    fn add(self, other: Vast2) -> Vast2 {
        unimplemented!();
    }
}
