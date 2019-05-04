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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShapeId(pub u32);

#[derive(Debug)]
pub struct Shape {
    // Gameplay:
    pub id: ShapeId,

    // TODO: use a faster hash table or just a vec https://github.com/servo/rust-fnv
    pub parent_ids: HashMap<ShapeId, ChildPoint>,
    // TODO: instead store a distinguished parent?
    pub polyomino: Polyomino,

    // Drawing:
    pub fill_color: [f32; 3],
    pub outline_color: [f32; 3],
}

impl Shape {
    pub fn has_position(&self, p: UPoint) -> bool {
        self.polyomino.has_position(p.to_untyped())
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
        self.id == other.id && self.parent_ids == other.parent_ids
    }
}

impl Eq for Shape {}

#[derive(PartialEq, Eq, Debug)]
enum Neighbour {
    OOB,
    Unoccupied(Location),
    Occupied(Location, Square),
}

trait HasSquares {
    // TODO: Would be nice if this could be impl Iterator
    #[inline]
    fn constituent_squares<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<Iterator<Item = Square> + 'a>;

    #[inline]
    fn constituent_locations<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<Iterator<Item = Location> + 'a> {
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
    fn locations<'a>(&'a self, universe: &'a Universe) -> Box<Iterator<Item = Location> + 'a> {
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
        self.squares_with_fringe(universe, d)
            .all(|(_, neighbour)| match neighbour {
                Neighbour::OOB => false,
                Neighbour::Unoccupied(_) => true,
                Neighbour::Occupied(_, _) => false,
            })
    }

    // This means, all the locations one step in direction d from self
    // The funny name hopefully makes the order harder to confuse
    #[inline]
    fn squares_with_fringe<'a>(
        &'a self,
        universe: &'a Universe,
        d: Direction,
    ) -> Box<Iterator<Item = (Square, Neighbour)> + 'a> {
        let self_squares: Vec<Square> = self.constituent_squares(universe).collect();
        Box::new(
            self.constituent_squares(universe).filter_map(move |s| {
                match s.location(universe).nudge(universe, d) {
                    None => Some((s, Neighbour::OOB)), // We hit out of bounds
                    Some(n) => match n.inhabitant(universe) {
                        None => Some((s, Neighbour::Unoccupied(n))), // We are next to empty space
                        Some(i) => {
                            if self_squares.contains(&i) {
                                None // We are still in self
                            } else {
                                Some((s, Neighbour::Occupied(n, i))) // We are next to some other shape
                            }
                        }
                    },
                }
            }),
            // TODO: this is doing more calculation than necessary,
            // for example, a polyomino knows which squares are on the
            // edge
        )
    }
}

trait HasShapes {}

impl HasSquares for Shape {
    fn constituent_squares<'a>(
        &'a self,
        _universe: &'a Universe,
    ) -> Box<Iterator<Item = Square> + 'a> {
        Box::new(self.polyomino.squares.iter().map(move |p| Square {
            shape_id: self.id,
            position: UPoint::from_untyped(p),
        }))
    }

    // TODO: specialise some other methods? only if worth it
}

#[derive(PartialEq, Eq, Debug)]
pub struct Universe {
    pub shapes: BTreeMap<ShapeId, Shape>, // Should probably just be a Vec
}

impl Universe {
    pub fn minimal() -> Universe {
        let id1 = ShapeId(1);
        let id2 = ShapeId(2);
        let shape1 = Shape {
            id: id1,
            parent_ids: hashmap! { id2 => ChildPoint::new(1, 0) },
            polyomino: Polyomino::monomino(),
            fill_color: [0.5, 0.5, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };
        let shape2 = Shape {
            id: id2,
            parent_ids: hashmap! { id1 => ChildPoint::new(0, 0) },
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

    pub fn children_of_with_position<'a>(
        &'a self,
        parent_id: ShapeId,
    ) -> impl Iterator<Item = (ShapeId, ChildPoint)> + 'a {
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

    fn used_parents_of(&self, sid: ShapeId) -> Vec<ShapeId> {
        let shape = &self.shapes[&sid];
        shape
            .parent_ids
            .iter()
            .filter_map(|(&parent_id, &pos_on_parent)| {
                let habitat = shape
                    .polyomino
                    .translate(pos_on_parent.to_untyped().to_vector())
                    .divide_by(ZOOM_SCALE);
                if habitat.overlaps(&self.shapes[&parent_id].polyomino) {
                    Some(parent_id)
                } else {
                    None
                }
            })
            .collect()
    }

    fn clean_parents(&mut self, sid: ShapeId) -> () {
        let used = self.used_parents_of(sid);
        self.shapes
            .entry(sid)
            .and_modify(|s| (*s).parent_ids.retain(|pid, _| used.contains(pid)));
    }

    fn expand_parent_list(&mut self, shape_id: ShapeId, d: Direction) -> () {
        let fringe: Vec<(Square, Location)> = self.shapes[&shape_id]
            .squares_with_fringe(&self, d)
            .filter_map(move |(square, neighbour)| match neighbour {
                Neighbour::OOB => None,
                Neighbour::Unoccupied(l) => Some((square, l)),
                Neighbour::Occupied(l, _) => Some((square, l)),
            })
            .collect();
        for (o, f) in fringe {
            let o_location = o.location(self);
            if o_location.square.shape_id != f.square.shape_id {
                // Then there's a new potential parent to add:
                // The idea is that o_location + d = f, so

                let shape_position_on_f_parent = f.to_coordinate()
                    - d.to_vect()
                    - ChildVec::from_untyped(&o.position.to_vector().to_untyped());
                self.shapes.entry(shape_id).and_modify(|s| {
                    (*s).parent_ids
                        .insert(f.square.shape_id, shape_position_on_f_parent.to_point());
                });

                // TODO: this is possibly setting the same parent
                // repeatedly, but it should set it to the same thing
                // every time.
            }
        }
    }

    // CAUTION!! This invalidates the chunk and all the shapes in it
    pub fn do_shove(&mut self, chunk: TopChunk, d: Direction) -> () {
        // Add possible new parents in that direction
        for &sid in chunk.top_shape_ids.keys() {
            self.expand_parent_list(sid, d);
        }

        // Move every shape on its parent
        for sid in chunk.top_shape_ids.keys() {
            let mut result = hashmap![];
            for (pid, pos) in self.shapes[sid].parent_ids.iter() {
                result.insert(*pid, *pos + d.to_vect());
            }
            self.shapes
                .entry(*sid)
                .and_modify(|p| (*p).parent_ids = result);
        }

        // MUST TODO: Figure out which parts of this chunk have split off. Or can this never happen?
        for sid in chunk.top_shape_ids.keys() {
            self.clean_parents(*sid);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Square {
    pub shape_id: ShapeId,
    pub position: UPoint,
}

impl Square {
    fn location_on(&self, universe: &Universe, parent_id: ShapeId) -> Option<Location> {
        let shape = &universe.shapes[&self.shape_id];
        let parent = &universe.shapes[&parent_id];
        let pos_on = shape.parent_ids.get(&parent_id)?;
        let offset = self.position.to_vector() + math::coerce_up(pos_on.to_vector());
        let (new_pos, new_subp) = math::split_up(math::coerce_down(offset));

        if !parent
            .polyomino
            .has_position(new_pos.to_point().to_untyped())
        {
            return None;
        }

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

    pub fn is_unoccupied(&self, universe: &Universe) -> bool {
        !self.inhabitant(universe).is_some()
    }
}

impl Square {
    fn nudge_easy(&self, u: &Universe, d: Direction) -> Option<Square> {
        let newpos = self.position + d.to_vect();
        if u.shapes[&self.shape_id]
            .polyomino
            .squares
            .contains(&newpos.to_untyped())
        {
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
        ChildVec::new(
            v.x.mod_floor(&(ZOOM_SCALE as i32)),
            v.y.mod_floor(&(ZOOM_SCALE as i32)),
        )
    }

    fn nudge_easy(&self, d: Direction) -> Option<Location> {
        let newsubpos = self.subposition + d.to_vect();
        if newsubpos == Location::wrap_subpos(newsubpos) {
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

        seen.insert((*self, d));
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

#[derive(Clone, Debug)]
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

    pub fn explore(&self, shape_id: ShapeId) -> TotalChunk {
        let mut result = BTreeMap::new();
        let mut queue = VecDeque::new();

        queue.push_back((shape_id, Delta::zero()));

        self.explore_step(&mut result, &mut queue);

        let top: BTreeMap<ShapeId, UVec> = result
            .iter()
            .filter_map(|(s, d)| {
                if d.zdelta == 0 {
                    Some((*s, d.to_uvec().unwrap()))
                } else {
                    None
                }
            })
            .collect();
        let lower: BTreeMap<ShapeId, Delta> = result
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

        let origin_id = *top.keys().min().unwrap();
        let origin_position = top[&origin_id];

        let top_offsetted = top
            .into_iter()
            .map(|(s, d)| (s, d - origin_position))
            .collect();
        let lower_offsetted = lower
            .into_iter()
            .map(|(s, d)| (s, Delta::from(-origin_position).append(&d)))
            .collect();

        TotalChunk {
            top_shape_ids: top_offsetted,
            lower_shape_ids: lower_offsetted,
            origin_id,
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

    pub fn delta_to_parent_of(&self, chunk: &TopChunk) -> Delta {
        let parent_chunk = self.parent_of(chunk);
        let origin_shape = &self.shapes[&chunk.origin_id];
        let origin_parent_id = origin_shape.first_parent_id();
        let parent_to_origin = origin_shape.parent_ids[&origin_shape.first_parent_id()];
        let parent_position_in_chunk = parent_chunk.top_shape_ids[&origin_parent_id];
        Delta::from(parent_to_origin.to_vector())
            .invert()
            .revert(&Delta::from(parent_position_in_chunk))
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TopChunk {
    pub origin_id: ShapeId,
    pub top_shape_ids: BTreeMap<ShapeId, UVec>,
}

impl TopChunk {
    pub fn bounding_box(&self, universe: &Universe) -> TypedRect<i32, UniverseSpace> {
        TypedRect::from_untyped(
            &self
                .top_shape_ids
                .iter()
                .map(|(shape_id, offset)| {
                    universe.shapes[shape_id]
                        .polyomino
                        .bounding_box()
                        .translate(&offset.to_untyped())
                })
                .fold1(|ref a, ref b| a.union(b))
                .unwrap(),
        )
    }

    fn common_shape_with(&self, other: &TopChunk) -> Option<ShapeId> {
        for k in self.top_shape_ids.keys() {
            if other.top_shape_ids.contains_key(k) {
                return Some(*k);
            }
        }
        None
    }

    pub fn recentering_for(&self, other: &TopChunk) -> TypedVector2D<i32, UniverseSpace> {
        let common = self.common_shape_with(other).unwrap();
        other.top_shape_ids[&common] - self.top_shape_ids[&common]
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

// This stores offsets of each shape in the region
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopRegion {
    pub origin_id: ShapeId,
    pub top_shape_ids: BTreeMap<ShapeId, UVec>,
}

impl HasSquares for TopRegion {
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

impl TopRegion {
    // This should really be part of a "HasSquares" trait
    pub fn bounding_box(&self, universe: &Universe) -> TypedRect<i32, UniverseSpace> {
        TypedRect::from_untyped(
            &self
                .top_shape_ids
                .iter()
                .map(|(shape_id, offset)| {
                    universe.shapes[shape_id]
                        .polyomino
                        .bounding_box()
                        .translate(&offset.to_untyped())
                })
                .fold1(|ref a, ref b| a.union(b))
                .unwrap(),
        )
    }
}

impl Universe {
    // Also returns the offset of the neighbour's origin relative to this shape's origin
    fn neighbouring_shapes_in_direction<'a>(
        &'a self,
        shape: &'a Shape,
        d: Direction,
    ) -> impl Iterator<Item = (ShapeId, UVec)> + 'a {
        shape
            .squares_with_fringe(&self, d)
            .filter_map(move |(square, neighbour)| match neighbour {
                Neighbour::OOB => None,
                Neighbour::Unoccupied(_) => None,
                Neighbour::Occupied(_, neighbour_square) => Some((square, neighbour_square)),
            })
            .map(move |(square, neighbour_square)|

                 // Reasoning:
                 // self_origin + square_position + direction.to_vect - neighbour_square_position = neighbour_shape_origin

                 (neighbour_square.shape_id, square.position + d.to_vect() - neighbour_square.position))
            .unique()
        // TODO: could do unique by shape id first
    }

    fn neighbouring_shapes_of(&self, shape: &Shape) -> Vec<(ShapeId, UVec)> {
        Direction::all()
            .iter()
            .flat_map(move |d| self.neighbouring_shapes_in_direction(shape, *d))
            .unique()
            .collect()
    }

    fn explore_region(&self, starting_shapes: &BTreeMap<ShapeId, UVec>) -> TopRegion {
        let mut iter = starting_shapes.iter().peekable();
        let (&origin_id, _) = iter.peek().unwrap();

        let mut stack: VecDeque<(ShapeId, UVec)> = VecDeque::new();
        let mut result: BTreeMap<ShapeId, UVec> = BTreeMap::new();

        for (&shape_id, &offset) in iter {
            stack.push_back((shape_id, offset));
            result.insert(shape_id, offset);
        }

        while stack.len() > 0 {
            let (next_id, next_offset) = stack.pop_front().unwrap();
            for (neighbour_id, neighbour_offset) in
                self.neighbouring_shapes_of(&self.shapes[&next_id])
            {
                if !result.contains_key(&neighbour_id) {
                    let new_offset = neighbour_offset + next_offset;
                    stack.push_back((neighbour_id, new_offset));
                    result.insert(neighbour_id, new_offset);
                }
            }
        }

        TopRegion {
            origin_id: origin_id,
            top_shape_ids: result,
        }
    }

    pub fn region_of(&self, shape: &Shape) -> TopRegion {
        let singleton = btreemap![shape.id => UVec::new(0, 0)];
        self.explore_region(&singleton)
    }

    // This could be part of a HasShapes trait
    pub fn region_of_chunk(&self, chunk: &TopChunk) -> TopRegion {
        self.explore_region(&chunk.top_shape_ids)
    }
}

#[derive(PartialEq, Eq, Debug)]
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
            player_chunk,
        }
    }

    pub fn do_zoom(&mut self) -> () {
        // TODO NLEVELS: If there are more than two levels, this won't work.
        self.player_chunk = self.universe.parent_of(&self.player_chunk);
    }

    pub fn can_move(&self, d: Direction) -> bool {
        self.player_chunk.unblocked(&self.universe, d)
    }

    pub fn do_move(&mut self, d: Direction) -> () {
        // TODO: could use mem::replace here to save copying the chunk
        // TODO: this doesn't account for chunks splitting in two? Or can that not happen?
        let some_player_shape_id = self.player_chunk.origin_id;

        self.universe.do_shove(self.player_chunk.clone(), d);
        self.player_chunk = self.universe.top_chunk_of_id(some_player_shape_id);

        //println!("{:#?}", self);
    }
}

// This represents a monotone path between chunks
#[derive(Debug, PartialEq, Eq)]
pub enum MonotonePath {
    Zero,
    Up { distance: u32 },
    Down { path: Vec<ShapeId> },
}

impl MonotonePath {
    pub fn zdelta(&self) -> i32 {
        use self::MonotonePath::*;
        match self {
            Zero => 0,
            Up { distance } => *distance as i32,
            Down { path } => -(path.len() as i32),
        }
    }

    pub fn take(&self, n: u32) -> MonotonePath {
        use self::MonotonePath::*;

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
        use self::MonotonePath::*;

        match *self {
            Zero => {
                assert!(n == 0);
                Zero
            }
            Up { distance } => {
                assert!(n <= distance);
                if distance == n {
                    Zero
                } else {
                    Up {
                        distance: distance - n,
                    }
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
        use self::MonotonePath::*;
        match *self {
            Zero => Up { distance: 1 },
            Up { distance } => Up {
                distance: distance + 1,
            },
            Down { ref path } => {
                if path.len() == 1 {
                    Zero
                } else {
                    let mut new_path = path.clone();
                    new_path.pop();
                    Down { path: new_path }
                }
            }
        }
    }

    pub fn down_target_to(&self, to: ShapeId) -> MonotonePath {
        // TODO: the target is ignored if we are actually going down
        use self::MonotonePath::*;
        match *self {
            Zero => Down { path: vec![to] },
            Up { distance } => {
                if distance == 1 {
                    Zero
                } else {
                    Up {
                        distance: distance - 1,
                    }
                }
            }
            Down { ref path } => {
                let mut new_path = path.clone();
                new_path.push(to);
                Down { path: new_path }
            }
        }
    }

    // MUST TODO: This doesn't take into account movement
    // Also returns the target chunk of the delta
    pub fn as_delta_from(&self, universe: &Universe, chunk: &TopChunk) -> (Delta, TopChunk) {
        use self::MonotonePath::*;
        match *self {
            Zero => (Delta::zero(), chunk.clone()),
            Up { distance } => {
                let mut result = Delta::zero();
                let mut current_chunk = chunk.clone();
                for _ in 0..distance {
                    let next_chunk = universe.parent_of(&current_chunk);
                    result = result.append(&universe.delta_to_parent_of(&current_chunk));
                    current_chunk = next_chunk;
                }
                (result, current_chunk)
            }
            Down { ref path } => unimplemented!(),
        }
    }
}
