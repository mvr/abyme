extern crate gfx;

use std::collections::HashMap;
use std::time;

use euclid::{TypedPoint2D, TypedRect, TypedSize2D, TypedTransform2D};
use gfx::handle::Buffer;
use gfx::pso::PipelineState;
use gfx::traits::FactoryExt;
use gfx::IndexBuffer;

use defs::*;
use delta::Delta;
use math;
use math::*;
use mesh_gen::*;
use polyomino::*;
use shape::*;
use camera::CameraState;

// TODO: this works under the assumption that the shapes being drawn
// on each level are all distinct. (which is true currently)
#[derive(Clone, Debug)]
struct LevelTracker {
    level: i32,
    transforms: HashMap<ShapeId, Delta>,
}

impl LevelTracker {
    pub fn from_chunk(chunk: &TopChunk) -> LevelTracker {
        let mut result = hashmap![];

        for (shape_id, uvec) in &chunk.top_shape_ids {
            result.insert(*shape_id, Delta::from(*uvec));
        }

        LevelTracker {
            level: 0,
            transforms: result,
        }
    }

    pub fn go_up(&self, universe: &Universe) -> LevelTracker {
        let mut result: HashMap<ShapeId, Delta> = hashmap![];

        for (shape_id, delta) in &self.transforms {
            let shape = &universe.shapes[shape_id];

            for parent in universe.parents_of(shape) {
                if result.contains_key(&parent.id) {
                    continue;
                }

                result.insert(
                    parent.id,
                    delta.revert(&Delta::from(parent.delta_to_child(shape))),
                );
            }
        }

        LevelTracker {
            level: self.level + 1,
            transforms: result,
        }
    }

    pub fn go_down(&self, universe: &Universe) -> LevelTracker {
        let mut result: HashMap<ShapeId, Delta> = hashmap![];

        for (shape_id, delta) in &self.transforms {
            let shape = &universe.shapes[shape_id];

            for child in universe.children_of(shape) {
                if result.contains_key(&child.id) {
                    continue;
                }

                result.insert(
                    child.id,
                    delta.append(&Delta::from(shape.delta_to_child(child))),
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
    MoveComplete {
        chunk: TopChunk,
        direction: Direction,
    },
}

impl MoveState {
    pub fn update(&self, time_delta: time::Duration) -> MoveState {
        if let MoveState::Moving {
            progress,
            chunk,
            direction,
        } = self
        {
            let newprogress = progress + math::time::duration_to_secs(time_delta);
            if newprogress >= 1.0 {
                return MoveState::MoveComplete {
                    chunk: chunk.clone(),
                    direction: *direction,
                };
            } else {
                return MoveState::Moving {
                    chunk: chunk.clone(),
                    direction: *direction,
                    progress: newprogress,
                };
            }
        }
        return self.clone();
    }

    pub fn move_complete(&self) -> bool {
        match *self {
            MoveState::MoveComplete { .. } => true,
            _ => false,
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

    logical_state: LogicalState,
    move_state: MoveState,
}

impl<R: gfx::Resources> Director<R> {
    pub fn new<F: gfx::traits::Factory<R>>(
        factory: &mut F,
        resolution: TypedSize2D<u32, ScreenSpace>,
    ) -> Director<R> {
        let logical_state = LogicalState::minimal();

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

        let store = Director::build_mesh_cache(&logical_state, factory);
        let pmb = factory.create_vertex_buffer(&store.poly_meshes.vertices);
        let pmib = factory.create_index_buffer(&store.poly_meshes.all_indices[..]);

        let camera_state = CameraState::new(&resolution, &logical_state);
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

            logical_state,
            move_state: MoveState::None,
        }
    }

    fn build_mesh_cache<F: gfx::traits::Factory<R>>(gs: &LogicalState, _factory: &mut F) -> MeshStore {
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
            let shape = &self.logical_state.universe.shapes[shape_id];
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
        let mut l = LevelTracker::from_chunk(&self.camera_state.current_chunk);

        for _ in 0..DRAW_DISTANCE_UP {
            l = l.go_up(&self.logical_state.universe);
            l.filter_nonvisible();
        }

        self.draw_level(encoder, target, &l);
        for _ in 0..(DRAW_DISTANCE_UP + DRAW_DISTANCE_DOWN) {
            l = l.go_down(&self.logical_state.universe);
            l.filter_nonvisible();
            self.draw_level(encoder, target, &l);
        }
    }

    pub fn do_zoom(&mut self) -> () {
        self.logical_state.do_zoom();
        self.camera_state.do_zoom(&self.logical_state);
    }

    pub fn try_start_move(&mut self, d: Direction) -> () {
        if let MoveState::Moving { .. } = self.move_state {
            return;
        }

        if self.logical_state.can_move(d) {
            self.move_state = MoveState::Moving {
                chunk: self.logical_state.player_chunk.clone(),
                direction: d,
                progress: 0.0,
            }
        } else {
            // TODO: visual indicator?
        }
    }

    pub fn do_move(&mut self, d: Direction) -> () {
        self.logical_state.do_move(d);
        self.camera_state.recenter(&self.logical_state.player_chunk);
    }

    pub fn update(&mut self, time_delta: time::Duration) -> () {
        self.camera_state.update(&self.logical_state, time_delta);
        self.move_state = self.move_state.update(time_delta);

        if self.move_state.move_complete() {
            match self.move_state {
                MoveState::MoveComplete { direction, .. } => self.do_move(direction),
                _ => {}
            }
            self.move_state = MoveState::None;
        }
    }
}
