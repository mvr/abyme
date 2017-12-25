extern crate cgmath;

use cgmath::*;
use types::*;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Polyomino {
    pub squares: Vec<IVec2>,
}

pub enum BorderType {
    Interior,
    Exterior,
}
pub struct BorderSegment(IVec2, IVec2, BorderType);

impl Polyomino {
    pub fn monomino() -> Polyomino {
        Polyomino { squares: vec![Vector2::new(0, 0)] }
    }
    // pub fn monomino() -> Polyomino {
    //     Polyomino { squares: vec![Vector2::new(0,0)] }
    // }

    pub fn has_position(&self, p: IVec2) -> bool {
        self.squares.contains(&p)
    }

    fn borders(&self) -> Vec<BorderSegment> {
        unimplemented!()
    }
}
