extern crate cgmath;

use cgmath::*;

#[derive(Debug, Clone)]
pub struct Polyomino {
    pub squares: Vec<Vector2<i16>>,
}

impl Polyomino {
    pub fn has_position(&self, p: &Vector2<i16>) -> bool {
        self.squares.contains(p)
    }
}
