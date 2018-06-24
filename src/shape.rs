// extern crate euclid;

use itertools::Itertools;
use std::collections::{BTreeMap, HashSet, VecDeque};

use euclid::{TypedRect, TypedVector2D};
use rug::Integer;
use num::Integer as IntegerTrait;

use defs::*;
use delta::*;
use math;
use math::Direction;
use polyomino::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShapeId(u32);

#[derive(Debug)]
pub struct Shape {
    // Gameplay:
    pub id: ShapeId,

    // TODO: use a faster hash table or just a vec https://github.com/servo/rust-fnv
    pub parent_ids: BTreeMap<ShapeId, ChildPoint>,
    // TODO: instead store a distinguished parent?
    pub polyomino: Polyomino,

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

    pub fn delta_from_parent(&self, parent: &Shape) -> Delta {
        let coords = self.parent_ids[&parent.id].to_vector();
        Delta {
            zdelta: -1,
            scale: -1,
            coords: TypedVector2D::new(Integer::from(coords.x), Integer::from(coords.y)),
        }
    }

    pub fn delta_to_child(&self, child: &Shape) -> Delta {
        child.delta_from_parent(self)
    }

    pub fn delta_to_parent(&self, parent: &Shape) -> Delta {
        self.delta_from_parent(parent).invert()
    }
}

impl PartialEq for Shape {
    fn eq(&self, other: &Shape) -> bool {
        self.id == other.id
    }
}

impl Eq for Shape {}

trait HasSquares {
    // TODO: Would be nice if this could be impl Iterator
    #[inline]
    fn constituent_squares<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<Iterator<Item = Square> + 'a>;

    #[inline]
    fn constituent_locations<'a>(&'a self, universe: &'a Universe) -> Box<Iterator<Item = Location> + 'a> {
        Box::new(
            self.constituent_squares(universe)
                .map(move |s| s.location(universe)),
        )
    }

    #[inline]
    fn constituent_shapes<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<Iterator<Item = ShapeId> + 'a> {
        Box::new(
            self.constituent_squares(universe)
                .map(|s| s.shape_id)
                .unique(),
        )
    }

    #[inline]
    fn locations<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<Iterator<Item = Location> + 'a> {
        Box::new(
            self.constituent_squares(universe)
                .cartesian_product(Location::all_subpositions())
                .map(|(s, subp)| Location {
                    square: s,
                    subposition: subp,
                }),
        )
    }

    #[inline]
    fn child_shapes<'a>(&'a self, universe: &'a Universe) -> Box<Iterator<Item = ShapeId> + 'a> {
        Box::new(
            self.locations(universe)
                .filter_map(move |l| l.inhabitant(universe))
                .map(|s| s.shape_id)
                .unique(),
        )
    }

    #[inline]
    fn unblocked(&self, universe: &Universe, d: Direction) -> bool {
        self.constituent_locations(universe)
            .map(|l| l.nudge(universe, d))
            .all(|o| o.is_some()) // This is doing a little more calculation than is necessary...
    }
    //     fringe u d a = (filter (not . inhabits u a) justs, length allMaybes /= length justs)
    // where allMaybes = fmap (nudgeLocation u d) $ locations u a
    //     justs = catMaybes allMaybes
}

impl HasSquares for Shape {
    fn constituent_squares<'a>(
        &'a self,
        _universe: &'a Universe,
    ) -> Box<Iterator<Item = Square> + 'a> {
        Box::new(self.polyomino.squares.iter().map(move |p| Square {
            shape_id: self.id,
            position: *p,
        }))
    }

    // TODO: specialise some other methods? only if worth it
}

pub struct Universe {
    pub shapes: BTreeMap<ShapeId, Shape>, // Should probably just be a Vec
}

impl Universe {
    pub fn minimal() -> Universe {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: btreemap!{ id2 => ChildPoint::new(1, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [0.5, 0.5, 1.0],
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

    pub fn children_of_with_position<'a>(&'a self, parent_id: ShapeId) -> impl Iterator<Item = (ShapeId, ChildPoint)> + 'a {
        self.shapes
            .values()
            .filter(move |s| s.parent_ids.contains_key(&parent_id.clone()))
            .map(move |s| (s.id, s.parent_ids[&parent_id.clone()]))
    }


    pub fn parents_with_position_of(&self, shape: &Shape) -> Vec<(&Shape, ChildPoint)> {
        shape
            .parent_ids
            .iter()
            .map(|(id, pos)| (&self.shapes[id], *pos))
            .collect()
    }

    // canPushChunk :: Universe -> Direction -> Chunk -> Bool
    //     canPushChunk u d c = not (oob || any (isInhabited u) fr)
    // where (fr, oob) = fringe u d c

    // pub fn can_shove(&self, chunk: TopChunk, d: Direction) -> bool {
    //     unimplemented!();
    // }

    pub fn do_shove(&mut self, chunk: TopChunk, d: Direction) -> () {
        unimplemented!();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Square {
    pub shape_id: ShapeId,
    pub position: UPoint,
}

impl Square {
    fn location_on(&self, universe: &Universe, parent_id: ShapeId) -> Option<Location> {
        let pos_on = universe.shapes[&self.shape_id].parent_ids.get(&parent_id)?;
        let offset = self.position.to_vector() + math::coerce_up(pos_on.to_vector());
        let (new_pos, new_subp) = math::split_up(math::coerce_down(offset));

        Some(Location {
            square: Square {
                shape_id: parent_id,
                position: new_pos.to_point(),
            },
            subposition: new_subp,
        })
    }

    fn location(&self, universe: &Universe) -> Location {
        // TODO: This sort of sanity checking is slower, probably doesn't matter
        let mut locations: Vec<Location> = universe.shapes[&self.shape_id]
            .parent_ids
            .keys()
            .filter_map(|s| self.location_on(universe, *s))
            .collect();

        match locations.len() {
            0 => panic!("A square is not sitting on a location!"),
            1 => locations.pop().unwrap(),
            _ => panic!("A square is sitting on multiple locations!"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
    pub square: Square,
    pub subposition: ChildVec,
}

impl Location {
    pub fn inhabitant(&self, universe: &Universe) -> Option<Square> {
        let c = self.to_coordinate();

        let mut inhabitants: Vec<Square> = universe
            .children_of_with_position(self.square.shape_id)
            .into_iter()
            .filter(|&(sid, pos)| {
                let cpos = math::coerce_up(c - pos.to_vector());
                universe.shapes[&sid].has_position(cpos.to_point())
            })
            .map(|(sid, pos)| {
                let cpos = math::coerce_up(c - pos.to_vector());
                Square {
                    shape_id: sid,
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
        math::shift_down(self.square.position.to_vector()) + self.subposition
    }

    pub fn all_subpositions() -> Vec<ChildVec> {
        (0..ZOOM_SCALE)
            .cartesian_product(0..ZOOM_SCALE)
            .map(|(x, y)| ChildVec::new(x as i32, y as i32))
            .collect()
    }

    fn wrap_subposition(v: ChildVec) -> ChildVec {
        ChildVec::new(v.x % ZOOM_SCALE as i32, v.y % ZOOM_SCALE as i32)
    }
}

impl Square {
    fn nudge_easy(&self, u: &Universe, d: Direction) -> Option<Square> {
        let newpos = self.position + d.to_vect();
        if u.shapes[&self.shape_id].polyomino.squares.contains(&newpos) {
            Some(Square {
                shape_id: self.shape_id,
                position: newpos,
            })
        } else {
            None
        }
    }

    fn nudge_recurse(
        &self,
        universe: &Universe,
        d: Direction,
        seen: HashSet<(Location, Direction)>,
    ) -> Option<Square> {
        if let Some(easy) = self.nudge_easy(universe, d) {
            return Some(easy);
        }

        self.location(universe)
            .nudge_recurse(universe, d, seen)?
            .inhabitant(universe)
    }

    pub fn nudge(&self, universe: &Universe, d: Direction) -> Option<Square> {
        self.nudge_recurse(universe, d, HashSet::new())
    }
}

impl Location {
    fn wrap_subpos(v: ChildVec) -> ChildVec {
        ChildVec::new(v.x.mod_floor(&(ZOOM_SCALE as i32)),
                      v.y.mod_floor(&(ZOOM_SCALE as i32)))
    }

    fn nudge_easy(&self, d: Direction) -> Option<Location> {
        let newsubpos = self.subposition + d.to_vect();
        if newsubpos == Location::wrap_subpos(newsubpos)
        {
            Some(Location {
                square: self.square.clone(),
                subposition: newsubpos,
            })
        } else {
            None
        }
    }

    fn nudge_recurse(
        &self,
        universe: &Universe,
        d: Direction,
        mut seen: HashSet<(Location, Direction)>,
    ) -> Option<Location> {
        if seen.contains(&(*self, d)) {
            return None;
        }
        if let Some(easy) = self.nudge_easy(d) {
            return Some(easy);
        }

        seen.insert((self.clone(), d));
        let newsquare = self.square.nudge_recurse(universe, d, seen)?;
        let newsubpos = Location::wrap_subpos(self.subposition + d.to_vect());
        Some(Location {
            square: newsquare,
            subposition: newsubpos,
        })
    }

    pub fn nudge(&self, universe: &Universe, d: Direction) -> Option<Location> {
        self.nudge_recurse(universe, d, HashSet::new())
    }
}

// This stores offsets of each shape in the chunk

// TODO: these offsets may be too small, need a vec of offsets, one
// for each level (that can be converted to a normal form, if we want)
#[derive(Clone)]
pub struct TotalChunk {
    pub origin_id: ShapeId,
    pub top_shape_ids: BTreeMap<ShapeId, UVec>,
    pub lower_shape_ids: BTreeMap<ShapeId, Delta>,
}

impl TotalChunk {}

type ExploreResult = BTreeMap<ShapeId, Delta>;
type ExploreQueue = VecDeque<(ShapeId, Delta)>;

impl Universe {
    fn explore_step(&self, result: &mut ExploreResult, queue: &mut ExploreQueue) -> () {
        while !queue.is_empty() {
            let (sid, delta) = queue.pop_front().unwrap();
            let s = &self.shapes[&sid];

            let needs_update: bool = match result.get(&sid) {
                None => true,
                Some(existing) => delta.zdelta > existing.zdelta,
            };

            if !needs_update {
                continue;
            }

            result.insert(sid, delta.clone());

            if delta.zdelta < 0 {
                for p in self.parents_of_id(sid) {
                    queue.push_front((p.id, delta.revert(&s.delta_from_parent(p)))); // TODO: check this logic
                }
            }

            for c in self.children_of_id(sid) {
                queue.push_back((c.id, delta.append(&s.delta_to_child(c))));
            }
        }
    }

    fn explore(&self, shape_id: ShapeId) -> TotalChunk {
        let mut result = BTreeMap::new();
        let mut queue = VecDeque::new();

        queue.push_back((shape_id, Delta::zero()));

        self.explore_step(&mut result, &mut queue);

        let top = result
            .iter()
            .filter_map(|(s, d)| {
                if d.zdelta == 0 {
                    Some((*s, d.to_uvec().unwrap()))
                } else {
                    None
                }
            })
            .collect();
        let lower = result
            .iter()
            .filter_map(
                |(s, d)| {
                    if d.zdelta < 0 {
                        Some((*s, d.clone()))
                    } else {
                        None
                    }
                }, // More copying than necessary?
            )
            .collect();

        TotalChunk {
            origin_id: shape_id,
            top_shape_ids: top,
            lower_shape_ids: lower,
        }
    }

    // fn explore_parent_of(&self, shape_ids: BTreeMap<ShapeId, IVec2>) -> Chunk {
    // }

    pub fn chunk_of(&self, shape: &Shape) -> TotalChunk {
        self.explore(shape.id)
    }

    pub fn top_chunk_of_id(&self, id: ShapeId) -> TopChunk {
        TopChunk::from(self.explore(id))
    }

    pub fn parent_of(&self, chunk: &TopChunk) -> TopChunk {
        let origin_parent_id = self.shapes[&chunk.origin_id].first_parent_id();
        self.explore(origin_parent_id).into()
    }
}

#[derive(Debug, Clone)]
pub struct TopChunk {
    pub origin_id: ShapeId,
    pub top_shape_ids: BTreeMap<ShapeId, UVec>,
}

impl TopChunk {
    pub fn bounding_box(&self, universe: &Universe) -> TypedRect<i32, UniverseSpace> {
        let poly_bounds: Vec<TypedRect<i32, UniverseSpace>> = self
            .top_shape_ids
            .iter()
            .map(|(shape_id, offset)| {
                universe.shapes[shape_id]
                    .polyomino
                    .bounding_box()
                    .translate(offset)
            })
            .collect();
        poly_bounds.iter().fold(poly_bounds[0], |a, b| a.union(b))
    }
}

impl HasSquares for TopChunk {
    fn constituent_squares<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<Iterator<Item = Square> + 'a> {
        Box::new(
            self.top_shape_ids
                .keys()
                .map(move |id| &universe.shapes[id])
                .flat_map(move |s| s.constituent_squares(universe)),
        )
    }
}

impl From<TotalChunk> for TopChunk {
    fn from(t: TotalChunk) -> TopChunk {
        let TotalChunk {
            origin_id,
            top_shape_ids,
            ..
        } = t;
        TopChunk {
            origin_id,
            top_shape_ids,
        }
    }
}

pub struct LogicalState {
    pub universe: Universe,
    pub player_chunk: TopChunk,
}

impl LogicalState {
    pub fn minimal() -> LogicalState {
        let u = Universe::minimal();
        let player_chunk = u.top_chunk_of_id(ShapeId(1));

        LogicalState {
            universe: u,
            player_chunk: player_chunk,
        }
    }

    pub fn do_zoom(&mut self) -> () {
        self.player_chunk = self.universe.parent_of(&self.player_chunk);
    }

    pub fn can_move(&self, d: Direction) -> bool {
        self.player_chunk.unblocked(&self.universe, d)
    }

    pub fn do_move(&mut self, d: Direction) -> () {
        unimplemented!();
    }
}

// This has to represent a monotone path between chunks
#[derive(Debug)]
pub enum MonotonePath {
    Zero,
    Up { distance: u32 },
    Down { path: Vec<ShapeId> },
}

impl MonotonePath {
    pub fn take(&self, n: u32) -> MonotonePath {
        use MonotonePath::*;

        if n == 0 {
            return Zero;
        }

        match *self {
            Zero => {
                assert!(n == 0);
                Zero
            }
            Up { distance } => {
                assert!(n <= distance);
                Up { distance: n }
            }
            Down { ref path } => {
                assert!(n <= path.len() as u32);
                Down {
                    path: path[0..n as usize].to_vec(),
                }
            }
        }
    }

    // pub fn last(&self, n: u32) -> MonotonePath {
    //     use MonotonePath::*;

    //     if n == 0 { return Zero; }

    //     match *self {
    //         Zero => {
    //             assert!(n == 0);
    //             Zero
    //         }
    //         Up { distance } => {
    //             assert!(n <= distance);
    //             Up { distance: n }
    //         }
    //         Down { ref path } => {
    //             assert!(n <= path.len() as u32);
    //             Down {
    //                 path: path[path.len() - n as usize..].to_vec()
    //             }
    //         }
    //     }
    // }

    pub fn drop(&self, n: u32) -> MonotonePath {
        use MonotonePath::*;

        match *self {
            Zero => {
                assert!(n == 0);
                Zero
            }
            Up { distance } => {
                assert!(n <= distance);
                Up {
                    distance: distance - n,
                }
            }
            Down { ref path } => {
                assert!(n <= path.len() as u32);
                Down {
                    path: path[n as usize..].to_vec(),
                }
            }
        }
    }

    pub fn up_target(&self) -> MonotonePath {
        use MonotonePath::*;
        match *self {
            Zero => Up { distance: 1 },
            Up { distance } => Up {
                distance: distance + 1,
            },
            Down { ref path } => if path.len() == 1 {
                Zero
            } else {
                let mut new_path = path.clone();
                new_path.pop();
                Down { path: new_path }
            },
        }
    }

    // MUST TODO: This doesn't take into account movement
    // MUST TODO: This should be fixed to always give the delta to the origin of the chunk
    // Also returns the target shape of the delta
    pub fn as_delta_from(&self, universe: &Universe, id: ShapeId) -> (Delta, ShapeId) {
        use MonotonePath::*;
        match *self {
            Zero => (Delta::zero(), id),
            Up { distance } => {
                let mut result = Delta::zero();
                let mut current_shape_id = id;
                for _ in 0..distance {
                    let next_shape_id = universe.shapes[&id].first_parent_id();
                    let parent = &universe.shapes[&next_shape_id];
                    result = result.append(&universe.shapes[&id].delta_to_parent(parent));
                    current_shape_id = next_shape_id;
                }
                (result, current_shape_id)
            }
            Down { ref path } => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod movement_tests {
    use super::*;

    fn setup() -> LogicalState {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: btreemap!{ id2 => ChildPoint::new(1, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [0.5, 0.5, 1.0],
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

        let u = Universe { shapes: shapes };

        let player_chunk = u.top_chunk_of_id(ShapeId(1));

        LogicalState {
            universe: u,
            player_chunk: player_chunk,
        }
    }

    #[test]
    fn inhabitant_1() {
        let gs = setup();

        let l = Location {
            square: Square {
                shape_id: ShapeId(
                    1
                ),
                position: UVec::new(0,0).to_point()
            },
            subposition: ChildVec::new(1,0)
        };

        assert!(l.inhabitant(&gs.universe) == None);
    }


    #[test]
    fn test_1() {
        let gs = setup();

        assert!(!gs.can_move(Direction::Right));
    }
}
