extern crate cgmath;

use cgmath::*;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Polyomino {
    pub squares: Vec<Vector2<i16>>,
}

pub enum BorderType {
    Interior,
    Exterior,
}
pub struct BorderSegment(Vector2<i16>, Vector2<i16>, BorderType);

impl Polyomino {
    pub fn monomino() -> Polyomino {
        Polyomino { squares: vec![Vector2::new(0, 0)] }
    }
    // pub fn monomino() -> Polyomino {
    //     Polyomino { squares: vec![Vector2::new(0,0)] }
    // }

    pub fn has_position(&self, p: &Vector2<i16>) -> bool {
        self.squares.contains(p)
    }

    fn borders(&self) -> Vec<BorderSegment> {
        unimplemented!()
    }
}
