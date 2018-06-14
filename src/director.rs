extern crate gfx;

use std::collections::HashMap;
use std::ops::{Add, Div};
use std::time;

use euclid::{TypedPoint2D, TypedRect, TypedSize2D, TypedTransform2D, TypedVector2D};
use gfx::handle::Buffer;
use gfx::pso::PipelineState;
use gfx::traits::FactoryExt;
use gfx::IndexBuffer;

use defs::*;
use delta::{Delta, FractionalDelta};
use math;
use math::*;
use mesh_gen::*;
use polyomino::*;
use shape::*;

// TODO: Do I need a custom AATransform that manages precision
// somehow?

// TODO: think about how movement will work: this is tricky. When a
// chunk is broken in to pieces, we need to make sure our
// current_shape and target_shape are switched to being ones that are
// in the correct chunk
struct CameraState {
    camera_bounds: TypedRect<f32, DrawSpace>,

    current_chunk: TopChunk,
    current_transform: TypedTransform2D<f32, UniverseSpace, DrawSpace>,

    target_chunk: TopChunk,
    target_transform: TypedTransform2D<f32, UniverseSpace, DrawSpace>,

    current_to_target_path: MonotonePath, // This should stay in sync with the above...
}

impl CameraState {
    fn center<T, U>(rect: &TypedRect<T, U>) -> TypedPoint2D<T, U>
    where
        T: Copy + Add<T, Output = T> + Div<T, Output = T> + From<f32>,
    {
        TypedPoint2D::new(
            rect.origin.x + rect.size.width / T::from(2.0),
            rect.origin.y + rect.size.height / T::from(2.0),
        )
    }

    fn target_transform_for(
        chunk: &TopChunk,
        game_state: &GameState,
        bounds: TypedRect<f32, DrawSpace>,
    ) -> TypedTransform2D<f32, UniverseSpace, DrawSpace> {
        let chunk_bounds = chunk.bounding_box(&game_state.universe);

        transform::fit_rect_in(&chunk_bounds.to_f32().inflate(1.0, 1.0), &bounds)
    }

    pub fn new(resolution: &TypedSize2D<u32, ScreenSpace>, game_state: &GameState) -> CameraState {
        let camera_bounds = TypedRect::new(
            TypedPoint2D::zero(),
            TypedSize2D::new(resolution.width as f32, resolution.height as f32),
        );

        let chunk = &game_state.player_chunk;
        let transform = CameraState::target_transform_for(&chunk, game_state, camera_bounds);

        CameraState {
            camera_bounds: camera_bounds,

            current_chunk: chunk.clone(),
            current_transform: transform,

            target_chunk: chunk.clone(),
            target_transform: transform,

            current_to_target_path: MonotonePath::Zero,
        }
    }

    pub fn do_zoom(&mut self, game_state: &GameState) -> () {
        self.target_chunk = game_state.player_chunk.clone();
        self.target_transform = CameraState::target_transform_for(
            &game_state.player_chunk,
            game_state,
            self.camera_bounds,
        );

        self.current_to_target_path = self.current_to_target_path.up_target();
    }

    fn true_target_transform(
        &self,
        game_state: &GameState,
    ) -> TypedTransform2D<f32, UniverseSpace, DrawSpace> {
        let (delta, target_id) = self
            .current_to_target_path
            .as_delta_from(&game_state.universe, self.current_chunk.origin_id);
        delta
            .invert()
            .to_scale_transform()
            .post_mul(&self.target_transform)
    }

    pub fn update(&mut self, game_state: &GameState, time_delta: time::Duration) -> () {
        self.current_transform = transform::lerp(
            &self.current_transform,
            &self.true_target_transform(game_state),
            1.0 - (-math::time::duration_to_secs(time_delta) * CAMERA_LERP_SPEED).exp(),
        );

        self.normalise(game_state);
    }

    fn normalise(&mut self, game_state: &GameState) -> () {
        let scale = transform::scale(&self.true_target_transform(game_state));

        if scale > CAMERA_UPPER_NORMALISE_TRIGGER || scale < CAMERA_LOWER_NORMALISE_TRIGGER {
            let adjustment = self.current_to_target_path.take(1).as_delta_from(&game_state.universe, self.current_chunk.origin_id).0.to_scale_transform();

            self.current_transform = self.current_transform.pre_mul(&adjustment.inverse().unwrap());
            self.current_to_target_path = self.current_to_target_path.drop(1);
        }
    }


    pub fn set_origin_shape(&mut self, new_origin: &ShapeId) -> () {
        unimplemented!();
    }

    // This should/could rely on the focus of the camera not being the
    // player, but the parent of the player
    pub fn recenter(&mut self, new_target: &TopChunk) -> () {
        unimplemented!();
    }
}

// TODO: this works under the assumption that the shapes being drawn
// on each level are all distinct. (which is true currently)
#[derive(Clone)]
struct LevelTracker {
    level: i32,
    transforms: HashMap<ShapeId, FractionalDelta>,
}

impl LevelTracker {
    pub fn from_chunk(chunk: &TopChunk) -> LevelTracker {
        let mut result = hashmap![];

        for (shape_id, uvec) in &chunk.top_shape_ids {
            result.insert(*shape_id, FractionalDelta::from(*uvec));
        }

        LevelTracker {
            level: 0,
            transforms: result,
        }
    }

    pub fn go_down(&self, universe: &Universe) -> LevelTracker {
        let mut result: HashMap<ShapeId, FractionalDelta> = hashmap![];

        for (shape_id, delta) in &self.transforms {
            let shape = &universe.shapes[shape_id];

            for child in universe.children_of(shape) {
                if result.contains_key(&child.id) {
                    continue;
                }
                result.insert(
                    child.id,
                    delta.append(&FractionalDelta::from(shape.delta_to_child(child))),
                );
            }
        }

        LevelTracker {
            level: self.level + 1,
            transforms: result,
        }
    }

    pub fn go_up(&self, universe: &Universe) -> LevelTracker {
        let mut result: HashMap<ShapeId, FractionalDelta> = hashmap![];

        for (shape_id, delta) in &self.transforms {
            let shape = &universe.shapes[shape_id];

            for parent in universe.parents_of(shape) {
                if result.contains_key(&parent.id) {
                    continue;
                }
                result.insert(
                    parent.id,
                    delta.revert(&FractionalDelta::from(parent.delta_to_child(shape))),
                );
            }
        }

        LevelTracker {
            level: self.level - 1,
            transforms: result,
        }
    }

    pub fn filter_nonvisible(&mut self) -> () {
        // TODO: do this
    }
}

#[derive(Clone, Debug)]
pub enum MoveState {
    None,
    Moving {
        chunk: TopChunk,
        direction: Direction,
        progress: f32,
    },
}

impl MoveState {
    pub fn update(&mut self, time_delta: time::Duration) -> () {
        if let MoveState::Moving {
            ref mut progress, ..
        } = self
        {
            let newprogress = *progress + math::time::duration_to_secs(time_delta);
            if newprogress >= 0.0 {
                *self = MoveState::None
            } else {
                *progress = newprogress
            }
        }
    }

    pub fn move_complete(&self) -> bool {
        match *self {
            MoveState::None => false,
            MoveState::Moving { progress, .. } => progress > 1.0,
        }
    }
}

pub struct Director<R: gfx::Resources> {
    pub resolution: TypedSize2D<u32, ScreenSpace>,

    camera_state: CameraState,
    draw_space_to_gl: TypedTransform2D<f32, DrawSpace, GLSpace>,

    mesh_store: MeshStore,
    poly_mesh_buffer: Buffer<R, GpuShapeVertex>,
    poly_mesh_index_buffer: IndexBuffer<R>,
    shape_pso: PipelineState<R, shape_pipe::Meta>,

    game_state: GameState,
    move_state: MoveState,
}

impl<R: gfx::Resources> Director<R> {
    pub fn new<F: gfx::traits::Factory<R>>(
        factory: &mut F,
        resolution: TypedSize2D<u32, ScreenSpace>,
    ) -> Director<R> {
        let game_state = GameState::minimal();

        // TODO: will one day have to make this choose the right
        // shaders for the platform.
        let shape_pso = factory
            .create_pipeline_simple(
                include_bytes!(concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/src/shaders/shape_150.glslv"
                )),
                include_bytes!(concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/src/shaders/shape_150.glslf"
                )),
                shape_pipe::new(),
            )
            .unwrap();

        let store = Director::build_mesh_cache(&game_state, factory);
        let pmb = factory.create_vertex_buffer(&store.poly_meshes.vertices);
        let pmib = factory.create_index_buffer(&store.poly_meshes.all_indices[..]);

        let camera_state = CameraState::new(&resolution, &game_state);
        let ndc_bounds = TypedRect::new(TypedPoint2D::new(-1.0, -1.0), TypedSize2D::new(2.0, 2.0));
        let draw_space_to_gl = transform::rect_to_rect(&camera_state.camera_bounds, &ndc_bounds);

        Director {
            shape_pso,
            resolution,
            camera_state,
            draw_space_to_gl,

            mesh_store: store,
            poly_mesh_buffer: pmb,
            poly_mesh_index_buffer: pmib,

            game_state,
            move_state: MoveState::None,
        }
    }

    fn build_mesh_cache<F: gfx::traits::Factory<R>>(gs: &GameState, _factory: &mut F) -> MeshStore {
        let mut result = MeshStore::new();

        for s in gs.universe.shapes.values() {
            if result.contains_poly(&s.polyomino) {
                continue;
            }

            result.gen_polyomino_mesh(&s.polyomino);
        }

        result
    }

    fn execute_poly_draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        poly: &Polyomino,
        raw_transform: &TypedTransform2D<f32, UniverseSpace, GLSpace>,
        fill_color: [f32; 3],
        outline_color: [f32; 3],
    ) -> () {
        let arr_transform = (*raw_transform).to_gl_mat3();

        let fill_slice = self.mesh_store.poly_meshes.gfx_slice_for(
            &self.poly_mesh_index_buffer,
            PolyMeshId {
                poly: poly.clone(),
                which: PolyMeshType::FillMesh,
            },
        );

        let fill_data = shape_pipe::Data {
            vbuf: self.poly_mesh_buffer.clone(),
            out: target.clone(),
            transform: arr_transform,
            color: fill_color,
        };

        encoder.draw(&fill_slice, &self.shape_pso, &fill_data);

        let outline_slice = self.mesh_store.poly_meshes.gfx_slice_for(
            &self.poly_mesh_index_buffer,
            PolyMeshId {
                poly: poly.clone(),
                which: PolyMeshType::GridMesh,
            },
        );

        let outline_data = shape_pipe::Data {
            vbuf: self.poly_mesh_buffer.clone(),
            out: target.clone(),
            transform: arr_transform,
            color: outline_color,
        };

        //        encoder.draw(&outline_slice, &self.shape_pso, &outline_data);
    }

    fn draw_level<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        level_tracker: &LevelTracker,
    ) -> () {
        let transform_to_gl = self
            .camera_state
            .current_transform
            .post_mul(&self.draw_space_to_gl);

        for (shape_id, offset) in &level_tracker.transforms {
            let shape = &self.game_state.universe.shapes[shape_id];
            let offset_transform = offset.to_scale_transform();

            self.execute_poly_draw(
                encoder,
                target,
                &shape.polyomino,
                &offset_transform.post_mul(&transform_to_gl),
                shape.fill_color,
                shape.outline_color,
            );
        }
    }

    pub fn draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
    ) -> () {
        // TODO: move to defs.rs
        const DISTANCE_UP: u32 = 3;
        const DISTANCE_DOWN: u32 = 8;

        let mut l = LevelTracker::from_chunk(&self.camera_state.current_chunk);

        for _ in 0..DISTANCE_UP {
            l = l.go_up(&self.game_state.universe);
            l.filter_nonvisible();
        }

        self.draw_level(encoder, target, &l);

        for _ in 0..(DISTANCE_UP + DISTANCE_DOWN) {
            l = l.go_down(&self.game_state.universe);
            l.filter_nonvisible();
            self.draw_level(encoder, target, &l);
        }
    }

    pub fn do_zoom(&mut self) -> () {
        self.game_state.do_zoom();
        self.camera_state.do_zoom(&self.game_state);
    }

    pub fn try_start_move(&mut self, d: Direction) -> () {
        if let MoveState::Moving { .. } = self.move_state {
            return;
        }

        if self.game_state.can_move(d) {
            self.move_state = MoveState::Moving {
                chunk: self.game_state.player_chunk.clone(),
                direction: d,
                progress: 0.0,
            }
        } else {
            // TODO: visual indicator?
        }
    }

    pub fn do_move(&mut self, d: Direction) -> () {
        self.game_state.do_move(d);
        self.camera_state.recenter(&self.game_state.player_chunk);
    }

    pub fn update(&mut self, time_delta: time::Duration) -> () {
        self.camera_state.update(&self.game_state, time_delta);
        self.move_state.update(time_delta);

        if self.move_state.move_complete() {
            match self.move_state {
                MoveState::None => {}
                MoveState::Moving { direction, .. } => self.do_move(direction),
            }
            self.move_state = MoveState::None;
        }
    }
}
