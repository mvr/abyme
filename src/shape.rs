// extern crate euclid;

use std::collections::BTreeMap;
use std::collections::VecDeque;

use types::*;
use delta::*;
use polyomino::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShapeId(u32);

#[derive(Debug)]
pub struct Shape {
    // Gameplay:
    id: ShapeId,

    //TODO: use a faster hash table or just a vec https://github.com/servo/rust-fnv
    parent_ids: BTreeMap<ShapeId, ChildPoint>,

    pub polyomino: Polyomino,
    //     pub zoom_scale: i32, // TODO: keep constant?

    // Drawing:
    pub fill_color: [f32; 3],
    pub outline_color: [f32; 3],
}

impl Shape {
    pub fn has_position(&self, p: UPoint) -> bool {
        self.polyomino.has_position(p)
    }

    pub fn has_parent(&self, parent: &Shape) -> bool {
        self.parent_ids.contains_key(&parent.id)
    }

    pub fn first_parent_id(&self) -> ShapeId {
        let (&first_key, _) = self.parent_ids.iter().next().unwrap();
        first_key
    }

    pub fn position_on(&self, parent: &Shape) -> ChildPoint {
        self.parent_ids[&parent.id]
    }

    pub fn delta_to_parent(&self, parent: &Shape) -> Delta {
        unimplemented!();
    }

    pub fn delta_to_child(&self, child: &Shape) -> Delta {
        unimplemented!();
    }
}
impl PartialEq for Shape {
    fn eq(&self, other: &Shape) -> bool {
        self.id == other.id
    }
}

impl Eq for Shape {}

fn build_fill_mesh() -> () {}

pub struct Universe {
    pub shapes: BTreeMap<ShapeId, Shape>, // Should probably just be a Vec
}

impl Universe {
    pub fn minimal() -> Universe {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: btreemap!{ id2 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 1.0, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };
        let shape2 = Shape {
            id: id2,
            parent_ids: btreemap!{ id1 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };

        let mut shapes = BTreeMap::new();
        shapes.insert(id1, shape1);
        shapes.insert(id2, shape2);

        Universe { shapes: shapes }
    }

    pub fn parents_of<'a>(&'a self, shape: &'a Shape) -> impl Iterator<Item = &'a Shape> + 'a {
        shape.parent_ids.keys().map(move |id| &self.shapes[id])
    }

    pub fn parents_of_id<'a>(&'a self, shape_id: ShapeId) -> impl Iterator<Item = &'a Shape> {
        self.parents_of(&self.shapes[&shape_id])
    }

    pub fn children_of<'a>(&'a self, parent: &'a Shape) -> impl Iterator<Item = &'a Shape> {
        self.shapes.values().filter(move |s| s.has_parent(parent))
    }
    pub fn children_of_id<'a>(&'a self, parent_id: ShapeId) -> impl Iterator<Item = &'a Shape> {
        self.shapes
            .values()
            .filter(move |s| s.parent_ids.contains_key(&parent_id))
    }

    pub fn parents_with_position_of(&self, shape: &Shape) -> Vec<(&Shape, ChildPoint)> {
        shape
            .parent_ids
            .iter()
            .map(|(id, pos)| (&self.shapes[id], *pos))
            .collect()
    }
}

pub struct Square<'a> {
    pub universe: &'a Universe,
    pub shape: &'a Shape,
    pub position: UPoint,
}

impl<'a> Square<'a> {
    fn location_on(&self, parent_id: ShapeId) -> Option<Location> {
        let pos_on = self.shape.parent_ids.get(&parent_id)?;
        let new_shape = &self.universe.shapes[&parent_id];
        let offset = self.position.to_vector() + vectors::coerce_up(pos_on.to_vector());
        let (new_pos, new_subp) = vectors::split_up(vectors::coerce_down(offset));

        Some(Location {
            square: Square {
                universe: self.universe,
                shape: new_shape,
                position: new_pos.to_point(),
            },
            subposition: new_subp,
        })
    }

    fn location(&self) -> Location {
        // TODO: This sort of sanity checking is slower, probably doesn't matter
        let mut locations: Vec<Location> = self.shape
            .parent_ids
            .keys()
            .filter_map(|s| self.location_on(*s))
            .collect();

        match locations.len() {
            0 => panic!("A square is not sitting on a location!"),
            1 => locations.pop().unwrap(),
            _ => panic!("A square is sitting on multiple locations!"),
        }
    }
}

pub struct Location<'a> {
    pub square: Square<'a>,
    pub subposition: ChildVec,
}

impl<'a> Location<'a> {
    pub fn inhabitant(&self) -> Option<Square> {
        let c = self.to_coordinate();

        let mut inhabitants: Vec<Square> = self.square
            .universe
            .parents_with_position_of(self.square.shape)
            .into_iter()
            .filter(|&(s, pos)| {
                let cpos = vectors::coerce_up(c - pos.to_vector());
                s.has_position(cpos.to_point())
            })
            .map(|(s, pos)| {
                let cpos = vectors::coerce_up(c - pos.to_vector());
                Square {
                    universe: self.square.universe,
                    shape: s,
                    position: cpos.to_point(),
                }
            })
            .collect();

        if inhabitants.len() > 1 {
            // TODO: This sort of sanity checking is probably a bit slower
            panic!("A location has multiple inhabitants!");
        } else {
            inhabitants.pop()
        }
    }

    fn to_coordinate(&self) -> ChildVec {
        vectors::shift_down(self.square.position.to_vector()) + self.subposition
    }
}

// This stores offsets of each shape in the chunk

// TODO: these offsets may be too small, need a vec of offsets, one
// for each level (that can be converted to a normal form, if we want)
#[derive(Clone)]
pub struct Chunk {
    pub origin_id: ShapeId,
    pub top_shape_ids: BTreeMap<ShapeId, UVec>,
    pub lower_shape_ids: BTreeMap<ShapeId, Delta>,
}

impl Chunk {}

type ExploreResult = BTreeMap<ShapeId, Delta>;
type ExploreQueue = VecDeque<(ShapeId, Delta)>;

impl Universe {
    fn explore_step(&self, result: &mut ExploreResult, queue: &mut ExploreQueue) -> () {
        while !queue.is_empty() {
            let (sid, delta) = queue.pop_front().unwrap();
            let s = self.shapes.get(&sid).unwrap();

            let needs_update: bool = match result.get(&sid) {
                None => true,
                Some(existing) => delta.zdelta < existing.zdelta,
            };

            if !needs_update {
                continue;
            }

            result.insert(sid, delta.clone());

            if delta.zdelta > 0 {
                for p in self.parents_of_id(sid) {
                    queue.push_front((p.id, &delta + &s.delta_to_parent(p)));
                }
            }

            for c in self.children_of_id(sid) {
                queue.push_back((c.id, &delta + &s.delta_to_child(c)));
            }
        }
    }

    fn explore(&self, shape_id: ShapeId) -> Chunk {
        let mut result = BTreeMap::new();
        let mut queue = VecDeque::new();

        queue.push_back((shape_id, Delta::zero()));

        self.explore_step(&mut result, &mut queue);

        let top = result
            .iter()
            .filter_map(|(s, d)| {
                if d.zdelta == 0 {
                    Some((*s, d.to_uvec()))
                } else {
                    None
                }
            })
            .collect();
        let lower = result
            .iter()
            .filter_map(
                |(s, d)| {
                    if d.zdelta > 0 {
                        Some((*s, d.clone()))
                    } else {
                        None
                    }
                }, // More copying than necessary?
            )
            .collect();

        Chunk {
            origin_id: shape_id,
            top_shape_ids: top,
            lower_shape_ids: lower,
        }
    }

    // fn explore_parent_of(&self, shape_ids: BTreeMap<ShapeId, IVec2>) -> Chunk {
    // }

    pub fn chunk_of(&self, shape: &Shape) -> Chunk {
        self.explore(shape.id)
    }

    pub fn parent_of(&self, chunk: &Chunk) -> Chunk {
        let origin_parent_id = self.shapes[&chunk.origin_id].first_parent_id();
        self.explore(origin_parent_id)
    }
}

pub struct GameState {
    pub universe: Universe,
    pub player_chunk: Chunk,
}

impl GameState {
    pub fn minimal() -> GameState {
        let u = Universe::minimal();

        GameState {
            universe: u,
            player_chunk: Chunk {
                origin_id: ShapeId(1),
                top_shape_ids: btreemap![ShapeId(1) => UVec::zero()],
                lower_shape_ids: btreemap![ShapeId(2) => Delta::zero().shift_target_down()],
            },
        }
    }
}
