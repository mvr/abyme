// extern crate euclid;

pub use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

pub use euclid::{Point2D, TypedRect, TypedVector2D};
pub use num::Integer as IntegerTrait;
pub use rug::Integer;

use defs::*;
use delta::*;
use math;
use math::Direction;
use polyomino::*;

#[derive(Debug)]
pub struct Shape {
    pub polyomino: Polyomino,

    // Drawing:
    pub fill_color: [f32; 3],
    pub outline_color: [f32; 3],
}

impl PartialEq for Shape {
    fn eq(&self, other: &Shape) -> bool {
        self.polyomino == other.polyomino
    }
}

impl Eq for Shape {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChunkId(pub u32);

#[derive(PartialEq, Eq, Debug)]
pub struct Chunk {
    // Gameplay:
    pub id: ChunkId,
    pub parent_id: ChunkId,
    pub position_on_parent: ChildPoint,

    pub shapes: Vec<(Shape, UPoint)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Square {
    pub shape_id: ChunkId,
    pub position: UPoint,
}

impl Square {
    fn location(&self, universe: &Universe) -> Location {
        unimplemented!();
        // let mut locations: Vec<Location> = universe.shapes[&self.shape_id]
        //     .parent_ids
        //     .keys()
        //     .filter_map(|s| self.location_on(universe, *s))
        //     .collect();

        // match locations.len() {
        //     0 => panic!("A square is not sitting on a location!"),
        //     1 => locations.pop().unwrap(),
        //     _ => panic!("A square is sitting on multiple locations!"),
        // }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
    pub square: Square,
    pub subposition: ChildVec,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Universe {
    pub chunks: BTreeMap<ChunkId, Chunk>, // Should maybe just be a Vec
}

impl Universe {
    pub fn minimal() -> Universe {
        let id1 = ChunkId(1);
        let id2 = ChunkId(2);
        let chunk1 = Chunk {
            id: id1,
            parent_id: id2,
            position_on_parent: ChildPoint::new(0, 0),
            shapes: vec![(
                Shape {
                    polyomino: Polyomino::monomino(),
                    fill_color: [0.5, 0.5, 1.0],
                    outline_color: [0.5, 0.5, 0.5],
                },
                UPoint::new(0, 0),
            )],
        };

        let chunk2 = Chunk {
            id: id2,
            parent_id: id1,
            position_on_parent: ChildPoint::new(0, 0),
            shapes: vec![(
                Shape {
                    polyomino: Polyomino::monomino(),
                    fill_color: [1.0, 0.5, 0.5],
                    outline_color: [0.5, 0.25, 0.25],
                },
                UPoint::new(0, 0),
            )],
        };

        let mut chunks = BTreeMap::new();
        chunks.insert(id1, chunk1);
        chunks.insert(id2, chunk2);

        Universe { chunks: chunks }
    }
}
