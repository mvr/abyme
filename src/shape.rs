extern crate cgmath;

use std::collections::HashMap;
use cgmath::*;

use polyomino::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShapeId(u32);

#[derive(Debug, Clone)]
pub struct Shape {
    // Gameplay:
    pub id: ShapeId,
    pub parent_ids: Vec<(ShapeId, Vector2<i16>)>,
    pub polyomino: Polyomino,
    pub zoom_scale: i16,

    // Drawing:
}

pub enum BorderType {
    Interior,
    Exterior,
}
pub struct BorderSegment(Vector2<i16>, Vector2<i16>, BorderType);

impl Shape {
    fn has_position(&self, p: &Vector2<i16>) -> bool {
        self.polyomino.has_position(p)
    }

    fn borders(&self) -> Vec<BorderSegment> {
        unimplemented!()
    }
}

fn build_fill_mesh() -> () {}

pub struct Universe {
    pub shapes: HashMap<ShapeId, Shape>, // Should probably just be a Vec
}

impl Universe {
    pub fn unsafe_shape_at(&self, id: &ShapeId) -> &Shape {
        self.shapes.get(id).unwrap()
    }

    pub fn minimal() -> Universe {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: vec![(id2, Vector2::new(0, 0))],
            polyomino: Polyomino::monomino(),
            zoom_scale: 2,
        };
        let shape2 = Shape {
            id: id2,
            parent_ids: vec![(id1, Vector2::new(0, 0))],
            polyomino: Polyomino::monomino(),
            zoom_scale: 2,
        };

        let mut shapes = HashMap::new();
        shapes.insert(id1, shape1);
        shapes.insert(id2, shape2);

        Universe { shapes: shapes }
    }
}

pub struct Square<'a> {
    pub shape: &'a Shape,
    pub position: Vector2<i16>,
}

impl<'a> Square<'a> {
    fn location(&self) -> Location {
        unimplemented!();
    }
}

pub struct Location<'a> {
    pub square: &'a Square<'a>,
    pub subposition: Vector2<i16>,
}

impl<'a> Location<'a> {
    pub fn inhabitant(&self, universe: Universe) -> Option<Location> {
        unimplemented!();
    }

    fn to_position(&self) -> Vector2<i16> {
        (self.square.position * self.square.shape.zoom_scale) + self.subposition
    }
}

pub struct GameState {
    pub universe: Universe,
    pub player_chunk: ShapeId,
}

impl GameState {
    pub fn minimal() -> GameState {
        let u = Universe::minimal();

        GameState {
            universe: u,
            player_chunk: ShapeId(1),
        }
    }
}
