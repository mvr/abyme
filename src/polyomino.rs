extern crate euclid;

use types::*;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Polyomino {
    pub squares: Vec<UPoint>,
}

pub enum BorderType {
    Interior,
    Exterior,
}
pub struct BorderSegment(UPoint, UPoint, BorderType);

impl Polyomino {
    pub fn monomino() -> Polyomino {
        Polyomino { squares: vec![UPoint::new(0, 0)] }
    }
    // pub fn monomino() -> Polyomino {
    //     Polyomino { squares: vec![Vector2::new(0,0)] }
    // }

    pub fn has_position(&self, p: UPoint) -> bool {
        self.squares.contains(&p)
    }

    fn borders(&self) -> Vec<BorderSegment> {
        unimplemented!()
    }
}
