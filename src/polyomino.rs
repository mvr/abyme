extern crate euclid;

use euclid::TypedRect;
use euclid::TypedSize2D;

use defs::*;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Polyomino {
    pub squares: Vec<UPoint>,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum GridSegmentType {
    Perimeter,
    Internal,
}

pub struct GridSegment(pub UPoint, pub UPoint, pub GridSegmentType);

impl Polyomino {
    pub fn monomino() -> Polyomino {
        Polyomino {
            squares: vec![UPoint::new(0, 0)],
        }
    }

    pub fn has_position(&self, p: UPoint) -> bool {
        self.squares.contains(&p)
    }

    // TODO: This should be doing entire straight lines at once, not
    // just square by square

    // todo: The segment stuff could be more efficient, only building vecs
    // of the ones we want. Probably not a real issue.
    fn vertical_segments(&self) -> Vec<GridSegment> {
        // Vertical segments have a square to the right of them,
        // unless they're at the far-right of the polyominio.

        let mut res = vec![];

        for p in self.squares.iter().cloned() {
            let sort = if self.has_position(p + UVec::new(-1, 0)) {
                GridSegmentType::Internal
            } else {
                GridSegmentType::Perimeter
            };

            res.push(GridSegment(p, p + UVec::new(0, 1), sort));

            if !self.has_position(p + UVec::new(1, 0)) {
                // In this case we must be at the right-most side of the poly
                res.push(GridSegment(
                    p + UVec::new(1, 0),
                    p + UVec::new(1, 0) + UVec::new(0, 1),
                    GridSegmentType::Perimeter,
                ));
            }
        }

        res
    }

    fn horizontal_segments(&self) -> Vec<GridSegment> {
        // See above
        let mut res = vec![];

        for p in self.squares.iter().cloned() {
            let sort = if self.has_position(p + UVec::new(0, -1)) {
                GridSegmentType::Internal
            } else {
                GridSegmentType::Perimeter
            };

            res.push(GridSegment(p, p + UVec::new(1, 0), sort));

            if !self.has_position(p + UVec::new(0, 1)) {
                res.push(GridSegment(
                    p + UVec::new(0, 1),
                    p + UVec::new(0, 1) + UVec::new(1, 0),
                    GridSegmentType::Perimeter,
                ));
            }
        }

        res
    }

    pub fn segments(&self) -> Vec<GridSegment> {
        let mut r = self.vertical_segments();
        r.append(&mut self.horizontal_segments());
        r
    }

    pub fn square_rects(&self) -> Vec<TypedRect<i32, UniverseSpace>> {
        self.squares
            .iter()
            .map(|&s| TypedRect::new(s, TypedSize2D::new(1, 1)))
            .collect()
    }

    pub fn bounding_box(&self) -> TypedRect<i32, UniverseSpace> {
        let rects = self.square_rects();
        rects.iter().fold(rects[0], |a, b| a.union(b))
    }
}
