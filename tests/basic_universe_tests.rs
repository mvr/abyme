extern crate abyme;
#[macro_use]
extern crate maplit;

use std::collections::BTreeMap;

use abyme::defs::*;
use abyme::load_universe;
use abyme::math::Direction;
use abyme::polyomino::*;
use abyme::shape::*;

mod shape_tests {
    use super::*;

    #[test]
    fn inhabitant_1() {
        let gs =
            load_universe::load_universe(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test1.toml").to_string());

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
        let gs =
            load_universe::load_universe(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test1.toml").to_string());

        assert!(!gs.can_move(Direction::Right));
    }

    #[test]
    fn is_blocked_2() {
        let gs =
            load_universe::load_universe(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test2.toml").to_string());

        assert!(!gs.can_move(Direction::Up));
    }

    #[test]
    fn region_1() {
        let gs =
            load_universe::load_universe(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test2.toml").to_string());

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

    #[test]
    fn can_move_1() {
        let gs =
            load_universe::load_universe(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/test3.toml").to_string());

        assert!(gs.can_move(Direction::Right));
    }
}
