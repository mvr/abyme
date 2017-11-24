extern crate cgmath;

use std::collections::HashMap;
use cgmath::*;

use polyomino::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShapeId(u32);

#[derive(Debug)]
pub struct Shape {
    // Gameplay:
    id: ShapeId,

    //TODO: use a faster hash table or just a vec https://github.com/servo/rust-fnv
    parent_ids: HashMap<ShapeId, Vector2<i16>>,

    pub polyomino: Polyomino,
    pub zoom_scale: i16,

    // Drawing:
    pub fill_color: [f32; 3],
    pub outline_color: [f32; 3],
}

impl Shape {
    pub fn has_position(&self, p: Vector2<i16>) -> bool {
        self.polyomino.has_position(p)
    }

    pub fn position_on(&self, parent: &Shape) -> Vector2<i16> {
        self.parent_ids[&parent.id]
    }
}

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

    pub fn parents_of(&self, shape: &Shape) -> Vec<&Shape> {
        shape.parent_ids.keys().map(|id| &self.shapes[id]).collect()
    }

    pub fn parents_with_position_of(&self, shape: &Shape) -> Vec<(&Shape, Vector2<i16>)> {
        shape
            .parent_ids
            .iter()
            .map(|(id, pos)| (&self.shapes[id], *pos))
            .collect()
    }

    pub fn chunk_of(&self, shape: &Shape) -> Chunk {
        unimplemented!();
    }

    pub fn parent_of(&self, chunk: &Chunk) -> Chunk {
        unimplemented!();
    }
}

pub struct Square<'a> {
    pub universe: &'a Universe,
    pub shape: &'a Shape,
    pub position: Vector2<i16>,
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
        // TODO: This sort of sanity checking obviously slower, probably doesn't matter
        let locations: Vec<Location> = self.shape
            .parent_ids
            .keys()
            .filter_map(|s| self.location_on(*s))
            .collect();

        match locations.len() {
            0 => panic!("A square is not sitting on a location!"),
            1 => locations[0],
            _ => panic!("A square is sitting on multiple locations!"),
        }
    }
}

pub struct Location<'a> {
    pub square: Square<'a>,
    pub subposition: Vector2<i16>,
}

impl<'a> Location<'a> {
    pub fn inhabitant(&self) -> Option<Square> {
        let c = self.to_coordinate();

        let inhabitants: Vec<Square> = self.square
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

    fn to_coordinate(&self) -> Vector2<i16> {
        (self.square.position * self.square.shape.zoom_scale) + self.subposition
    }
}

pub struct Chunk {
    pub shape_ids: Vec<ShapeId>,
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
            player_chunk: Chunk { shape_ids: vec![ShapeId(1)] },
        }
    }
    // pub fn player_chunk(&self) -> &Shape {
    //     &self.universe.shapes[&self.player_chunk_id]
    // }
}
