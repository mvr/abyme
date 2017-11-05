extern crate cgmath;

use std::collections::HashMap;
use cgmath::*;

use polyomino::*;

#[derive(Debug, Clone, Copy)]
pub struct ShapeId(u32);

#[derive(Debug, Clone)]
pub struct Shape {
    // Gameplay:
    pub id: ShapeId,
    pub parent_ids: Vec<(ShapeId, Vector2<i16>)>,
    pub polyomino: Polyomino,
    pub zoom_scale: i16,
}

impl Shape {
    fn has_position(&self, p: &Vector2<i16>) -> bool {
        self.polyomino.has_position(p)
    }
}

fn build_fill_mesh() -> () {}

pub struct Universe {
    pub shapes: HashMap<ShapeId, Shape>, // Should probably just be a Vec
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

pub struct GameState<'a> {
    pub universe: Universe,
    pub player_chunk: &'a Shape,
}
