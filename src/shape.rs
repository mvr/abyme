extern crate cgmath;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use cgmath::*;

use types::*;
use polyomino::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShapeId(u32);

#[derive(Debug)]
pub struct Shape {
    // Gameplay:
    id: ShapeId,

    //TODO: use a faster hash table or just a vec https://github.com/servo/rust-fnv
    parent_ids: HashMap<ShapeId, IVec2>,

    pub polyomino: Polyomino,
    pub zoom_scale: i32,

    // Drawing:
    pub fill_color: [f32; 3],
    pub outline_color: [f32; 3],
}

impl Shape {
    pub fn has_position(&self, p: IVec2) -> bool {
        self.polyomino.has_position(p)
    }

    pub fn has_parent(&self, parent: &Shape) -> bool {
        self.parent_ids.contains_key(&parent.id)
    }

    pub fn position_on(&self, parent: &Shape) -> IVec2 {
        self.parent_ids[&parent.id]
    }
}
impl PartialEq for Shape {
    fn eq(&self, other: &Shape) -> bool {
        self.id == other.id
    }
}

impl Eq for Shape { }

fn build_fill_mesh() -> () {}

pub struct Universe {
    pub shapes: HashMap<ShapeId, Shape>, // Should probably just be a Vec
}

impl Universe {
    pub fn minimal() -> Universe {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: hashmap!{ id2 => Vector2::new(0, 0) },
            polyomino: Polyomino::monomino(),
            zoom_scale: 2,
            fill_color: [1.0, 1.0, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };
        let shape2 = Shape {
            id: id2,
            parent_ids: hashmap!{ id1 => Vector2::new(0, 0) },
            polyomino: Polyomino::monomino(),
            zoom_scale: 2,
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };

        let mut shapes = HashMap::new();
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
        self.shapes.values().filter(move |s| {
            s.parent_ids.contains_key(&parent_id)
        })
    }


    pub fn parents_with_position_of(&self, shape: &Shape) -> Vec<(&Shape, IVec2)> {
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
    pub position: IVec2,
}

impl<'a> Square<'a> {
    fn location_on(&self, parent_id: ShapeId) -> Option<Location> {
        let pos_on = self.shape.parent_ids.get(&parent_id)?;
        let new_shape = &self.universe.shapes[&parent_id];
        let new_pos = (self.position + pos_on) / new_shape.zoom_scale;
        let new_subp = (self.position + pos_on) % new_shape.zoom_scale;

        Some(Location {
            square: Square {
                universe: self.universe,
                shape: new_shape,
                position: new_pos,
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
    pub subposition: IVec2,
}

impl<'a> Location<'a> {
    pub fn inhabitant(&self) -> Option<Square> {
        let c = self.to_coordinate();

        let mut inhabitants: Vec<Square> = self.square
            .universe
            .parents_with_position_of(self.square.shape)
            .into_iter()
            .filter(|&(s, pos)| s.has_position(c - pos))
            .map(|(s, pos)| {
                Square {
                    universe: self.square.universe,
                    shape: s,
                    position: c - pos,
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

    fn to_coordinate(&self) -> IVec2 {
        (self.square.position * self.square.shape.zoom_scale) + self.subposition
    }
}

// This stores offsets of each shape in the chunk

// TODO: these offsets may be too small, need a vec of offsets, one
// for each level (that can be converted to a normal form, if we want)
#[derive(Clone)]
pub struct Chunk {
    pub origin_id: ShapeId,
    pub top_shape_ids: HashSet<(ShapeId, IVec2)>,
    pub lower_shape_ids: HashSet<(ShapeId, IVec2)>,
}

impl Chunk {
}

impl Universe {
    type ExploreResult = HashMap<ShapeId, u16>;
    type ExploreQueue = HashMap<ShapeId, IVec2, u16>;
    fn explore_step(&self, result: &mut ExploreResult, queue: &mut ExploreQueue) -> () {
        while !queue.is_empty() {
            let (s, i) = queue.pop_front().unwrap();

            let needs_update: bool = match result.get(&s) {
                None => true,
                Some(i2) => i < *i2,
            };

            if !needs_update {
                continue;
            }

            result.insert(s, i);

            if i > 0 {
                for p in self.parents_of_id(s) {
                    queue.push_front((p.id, i - 1));
                }
            }

            for p in self.children_of_id(s) {
                queue.push_back((p.id, i + 1));
            }
        }
    }

    fn explore(&self, shape_ids: impl Iterator<Item = ShapeId>) -> Chunk {
        let mut result = HashMap::new();
        let mut queue = VecDeque::new();

        for sid in shape_ids {
            queue.push_back((sid, 0));
        }

        self.explore_step(&mut result, &mut queue);

        let top = result.iter().filter_map(
            |(s, d)| if *d == 0 { Some(*s) } else { None },
        ).collect();
        let lower = result.iter().filter_map(
            |(s, d)| if *d > 0 { Some(*s) } else { None },
        ).collect();

        Chunk {
            top_shape_ids: top,
            lower_shape_ids: lower
        }
    }

    pub fn chunk_of(&self, shape: &Shape) -> Chunk {
        self.explore(Some(shape.id).into_iter())
    }

    pub fn parent_of(&self, chunk: &Chunk) -> Chunk {
        self.explore(chunk.top_shape_ids.clone().into_iter())
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
                top_shape_ids: hashset![ShapeId(1)],
                lower_shape_ids: hashset![],
            },
        }
    }
}
