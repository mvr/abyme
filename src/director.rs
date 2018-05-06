extern crate gfx;

use std::collections::HashMap;
use std::collections::VecDeque;
use std::ops::{Add, Div};

use gfx::Slice;
use gfx::shade::ToUniform;
use gfx::handle::Buffer;
use gfx::traits::FactoryExt;
use gfx::pso::PipelineState;
use euclid::{TypedPoint2D, TypedRect, TypedSize2D, TypedTransform2D, TypedVector2D};

use defs::*;
use math::*;
use mesh_gen::*;
use delta::Delta;
use polyomino::*;
use shape::*;

// TODO: think about how movement will work: this is tricky. When a
// chunk is broken in to pieces, we need to make sure our
// current_shape and target_shape are switched to being ones that are
// in the correct chunk
struct CameraState {
    camera_bounds: TypedRect<f32, DrawSpace>,

    current_shape: ShapeId,
    current_transform: TypedTransform2D<f32, UniverseSpace, DrawSpace>,

    target_shape: ShapeId,
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

    pub fn new(resolution: &TypedSize2D<u32, ScreenSpace>, game_state: &GameState) -> CameraState {
        let camera_bounds = TypedRect::new(
            TypedPoint2D::zero(),
            TypedSize2D::new(resolution.width as f32, resolution.height as f32),
        );

        let chunk = &game_state.player_chunk;
        let chunk_bounds = chunk.bounding_box(&game_state.universe);

        let transform =
            transform::fit_rect_in(&chunk_bounds.to_f32().inflate(1.0, 1.0), &camera_bounds);

        CameraState {
            camera_bounds: camera_bounds,

            current_shape: chunk.origin_id,
            current_transform: transform,

            target_shape: chunk.origin_id,
            target_transform: transform,

            current_to_target_path: MonotonePath::Zero,
        }
    }
}

// TODO: this works under the assumption that the shapes being drawn
// on each level are all distinct. (which is true currently)
#[derive(Clone)]
struct LevelTracker {
    level: i32,
    transforms: HashMap<ShapeId, Delta>,
}

impl LevelTracker {
    pub fn from_chunk(chunk: &TopChunk) -> LevelTracker {
        let mut result = hashmap![];

        for (shape_id, uvec) in chunk.top_shape_ids.iter() {
            result.insert(*shape_id, Delta::from(*uvec));
        }

        LevelTracker {
            level: 0,
            transforms: result,
        }
    }

    pub fn go_down(&self, universe: &Universe) -> LevelTracker {
        let mut result: HashMap<ShapeId, Delta> = hashmap![];

        for (shape_id, delta) in self.transforms.iter() {
            let shape = &universe.shapes[shape_id];

            for child in universe.children_of(shape) {
                if result.contains_key(&child.id) {
                    continue;
                }
                result.insert(child.id, delta.append(&shape.delta_to_child(child)));
            }
        }

        LevelTracker {
            level: self.level + 1,
            transforms: result,
        }
    }

    pub fn go_up(&self, universe: &Universe) -> LevelTracker {
        let mut result: HashMap<ShapeId, Delta> = hashmap![];

        for (shape_id, delta) in self.transforms.iter() {
            let shape = &universe.shapes[shape_id];

            for parent in universe.parents_of(shape) {
                if result.contains_key(&parent.id) {
                    continue;
                }
                result.insert(parent.id, delta.revert(&parent.delta_to_child(&shape)));
            }
        }

        LevelTracker {
            level: self.level - 1,
            transforms: result,
        }
    }

    pub fn filter_nonvisible(&mut self) -> () {}
}

pub struct Director<'a, R: gfx::Resources> {
    pub resolution: TypedSize2D<u32, ScreenSpace>,

    camera_state: CameraState,
    draw_space_to_gl: TypedTransform2D<f32, DrawSpace, GLSpace>,

    mesh_store: MeshStore,
    poly_mesh_buffer: Buffer<R, GpuShapeVertex>,
    shape_pso: PipelineState<R, shape_pipe::Meta>,

    game_state: &'a GameState,
}

impl<'a, R: gfx::Resources> Director<'a, R> {
    pub fn new<F: gfx::traits::Factory<R>>(
        u: &'a GameState,
        factory: &mut F,
        resolution: TypedSize2D<u32, ScreenSpace>,
    ) -> Director<'a, R> {
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

        let store = Director::build_mesh_cache(u, factory);
        let pmb = factory.create_vertex_buffer(&store.poly_meshes.vertices);

        let camera_state = CameraState::new(&resolution, &u);
        let ndc_bounds = TypedRect::new(TypedPoint2D::new(-1.0, -1.0), TypedSize2D::new(2.0, 2.0));
        let draw_space_to_gl = transform::rect_to_rect(&camera_state.camera_bounds, &ndc_bounds);

        Director {
            shape_pso: shape_pso,

            resolution: resolution,

            camera_state: camera_state,
            draw_space_to_gl: draw_space_to_gl,

            game_state: u,
            mesh_store: store,
            poly_mesh_buffer: pmb,
        }
    }

    fn build_mesh_cache<F: gfx::traits::Factory<R>>(
        gs: &'a GameState,
        factory: &mut F,
    ) -> MeshStore {
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
        let fill_slice = self.mesh_store.poly_meshes.gfx_slice_for(PolyMeshId {
            poly: poly.clone(),
            which: PolyMeshType::GridMesh,
        });
        let arr_transform = (*raw_transform).to_gl_mat3();

        let fill_data = shape_pipe::Data {
            vbuf: self.poly_mesh_buffer.clone(),
            out: target.clone(),
            transform: arr_transform,
            color: fill_color,
        };

        encoder.draw(&fill_slice, &self.shape_pso, &fill_data);

        let outline_slice = self.mesh_store.poly_meshes.gfx_slice_for(PolyMeshId {
            poly: poly.clone(),
            which: PolyMeshType::GridMesh,
        });

        let outline_data = shape_pipe::Data {
            vbuf: self.poly_mesh_buffer.clone(),
            out: target.clone(),
            transform: arr_transform,
            color: outline_color,
        };

        encoder.draw(&outline_slice, &self.shape_pso, &outline_data);
    }

    fn draw_level<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        level_tracker: &LevelTracker,
    ) -> () {
        let transform_to_gl = self.camera_state
            .current_transform
            .post_mul(&self.draw_space_to_gl);

        for (shape_id, offset) in level_tracker.transforms.iter() {
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
        const DISTANCE_DOWN: u32 = 3;

        let mut l = LevelTracker::from_chunk(&self.game_state.player_chunk);

        for _ in 1..DISTANCE_UP {
            l = l.go_up(&self.game_state.universe);
            l.filter_nonvisible();
        }

        self.draw_level(encoder, target, &l);

        for _ in 1..(DISTANCE_UP + DISTANCE_DOWN) {
            l = l.go_down(&self.game_state.universe);
            l.filter_nonvisible();
            self.draw_level(encoder, target, &l);
        }
    }
}
