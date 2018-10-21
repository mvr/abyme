extern crate abyme;
#[macro_use]
extern crate maplit;

use std::collections::{BTreeMap};

use abyme::defs::*;
use abyme::delta::*;
use abyme::math;
use abyme::math::Direction;
use abyme::polyomino::*;
use abyme::shape::*;

mod shape_tests {
    use super::*;

    // TODO: replace this with loading from a toml
    fn setup_1() -> LogicalState {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: hashmap!{ id2 => ChildPoint::new(1, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [0.5, 0.5, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };
        let shape2 = Shape {
            id: id2,
            parent_ids: hashmap!{ id1 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };

        let mut shapes = BTreeMap::new();
        shapes.insert(id1, shape1);
        shapes.insert(id2, shape2);

        let u = Universe { shapes: shapes };

        let player_chunk = u.top_chunk_of_id(ShapeId(1));

        LogicalState {
            universe: u,
            player_chunk: player_chunk,
        }
    }

    #[test]
    fn inhabitant_1() {
        let gs = setup_1();

        let l = Location {
            square: Square {
                shape_id: ShapeId(1),
                position: UVec::new(0, 0).to_point(),
            },
            subposition: ChildVec::new(1, 0),
        };

        assert!(l.inhabitant(&gs.universe) == None);
    }

    #[test]
    fn is_blocked_1() {
        let gs = setup_1();

        assert!(!gs.can_move(Direction::Right));
    }

    fn setup_2() -> LogicalState {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let id3 = ShapeId(3);
        let shape1 = Shape {
            id: id1,
            parent_ids: hashmap!{ id2 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [0.5, 0.5, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };
        let shape2 = Shape {
            id: id2,
            parent_ids: hashmap!{ id1 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };
        let shape3 = Shape {
            id: id3,
            parent_ids: hashmap!{ id2 => ChildPoint::new(0, 1) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };

        let mut shapes = BTreeMap::new();
        shapes.insert(id1, shape1);
        shapes.insert(id2, shape2);
        shapes.insert(id3, shape3);

        let u = Universe { shapes: shapes };

        let player_chunk = u.top_chunk_of_id(ShapeId(1));

        LogicalState {
            universe: u,
            player_chunk: player_chunk,
        }
    }

    #[test]
    fn is_blocked_2() {
        let gs = setup_2();

        assert!(!gs.can_move(Direction::Up));
    }

    #[test]
    fn region_1() {
        let gs = setup_2();

        let shape1 = &gs.universe.shapes[&ShapeId(1)];
        let shape2 = &gs.universe.shapes[&ShapeId(2)];
        let shape3 = &gs.universe.shapes[&ShapeId(3)];

        assert_eq!(
            gs.universe.region_of(shape1),
            TopRegion {
                origin_id: ShapeId(1),
                top_shape_ids: btreemap![
                    ShapeId(1) => UVec::new(0, 0),
                    ShapeId(3) => UVec::new(0, 1),
                ],
            }
        );

        assert_eq!(
            gs.universe.region_of(shape3),
            TopRegion {
                origin_id: ShapeId(3),
                top_shape_ids: btreemap![
                    ShapeId(1) => UVec::new(0, -1),
                    ShapeId(3) => UVec::new(0, 0),
                ],
            }
        );

        assert_eq!(
            gs.universe.region_of(shape2),
            TopRegion {
                origin_id: ShapeId(2),
                top_shape_ids: btreemap![
                    ShapeId(2) => UVec::new(0, 0),
                ],
            }
        );
    }

    fn setup_3() -> LogicalState {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let id3 = ShapeId(3);
        let shape1 = Shape {
            id: id1,
            parent_ids: hashmap!{ id2 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::new(vec![Point2D::new(0, 0), Point2D::new(1, 0)]),
            fill_color: [0.5, 0.5, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };

        let shape2 = Shape {
            id: id2,
            parent_ids: hashmap!{ id1 => ChildPoint::new(0, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };
        let shape3 = Shape {
            id: id3,
            parent_ids: hashmap!{ id1 => ChildPoint::new(1, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [1.0, 0.5, 0.5],
            outline_color: [0.5, 0.25, 0.25],
        };

        let mut shapes = BTreeMap::new();
        shapes.insert(id1, shape1);
        shapes.insert(id2, shape2);
        shapes.insert(id3, shape3);

        let u = Universe { shapes: shapes };

        let player_chunk = u.top_chunk_of_id(ShapeId(1));

        LogicalState {
            universe: u,
            player_chunk: player_chunk,
        }
    }

    #[test]
    fn can_move_1() {
        let gs = setup_3();

        assert!(gs.can_move(Direction::Right));
    }
}
