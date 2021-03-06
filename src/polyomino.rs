extern crate euclid;

use euclid::{Point2D, Rect, Size2D, Vector2D};
use rug::ops::DivRounding;

use defs::*;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Polyomino {
    pub squares: Vec<Point2D<i32>>,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum GridSegmentType {
    Perimeter,
    Internal,
}

pub struct GridSegment(pub Point2D<i32>, pub Point2D<i32>, pub GridSegmentType);

impl Polyomino {
    pub fn new(squares: Vec<Point2D<i32>>) -> Polyomino {
        Polyomino { squares }
    }

    pub fn empty() -> Polyomino {
        Polyomino {
            squares: vec![],
        }
    }

    pub fn monomino() -> Polyomino {
        Polyomino {
            squares: vec![Point2D::new(0, 0)],
        }
    }

    pub fn has_position(&self, p: Point2D<i32>) -> bool {
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
            let sort = if self.has_position(p + Vector2D::new(-1, 0)) {
                GridSegmentType::Internal
            } else {
                GridSegmentType::Perimeter
            };

            res.push(GridSegment(p, p + Vector2D::new(0, 1), sort));

            if !self.has_position(p + Vector2D::new(1, 0)) {
                // In this case we must be at the right-most side of the poly
                res.push(GridSegment(
                    p + Vector2D::new(1, 0),
                    p + Vector2D::new(1, 0) + Vector2D::new(0, 1),
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
            let sort = if self.has_position(p + Vector2D::new(0, -1)) {
                GridSegmentType::Internal
            } else {
                GridSegmentType::Perimeter
            };

            res.push(GridSegment(p, p + Vector2D::new(1, 0), sort));

            if !self.has_position(p + Vector2D::new(0, 1)) {
                res.push(GridSegment(
                    p + Vector2D::new(0, 1),
                    p + Vector2D::new(0, 1) + Vector2D::new(1, 0),
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

    pub fn square_rects(&self) -> Vec<Rect<i32>> {
        self.squares
            .iter()
            .map(|&s| Rect::new(s, Size2D::new(1, 1)))
            .collect()
    }

    pub fn bounding_box(&self) -> Rect<i32> {
        let rects = self.square_rects();
        rects.iter().fold(rects[0], |a, b| a.union(b))
    }

    pub fn divide_by(&self, amount: u32) -> Polyomino {
        let mut result_squares = vec![];

        for s in &self.squares {
            let result_pos = Point2D::new(
                s.x.div_floor(&(amount as i32)),
                s.y.div_floor(&(amount as i32)),
            );

            if !result_squares.contains(&result_pos) {
                result_squares.push(result_pos);
            }
        }

        Polyomino {
            squares: result_squares,
        }
    }

    pub fn overlaps(&self, other: &Polyomino) -> bool {
        for s in &self.squares {
            if other.squares.contains(s) {
                return true;
            }
        }
        false
    }

    pub fn translate(&self, v: Vector2D<i32>) -> Polyomino {
        Polyomino {
            squares: self.squares.iter().map(|s| *s + v).collect(),
        }
    }
}
