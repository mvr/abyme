extern crate gfx;

use std::collections::HashMap;
use std::collections::VecDeque;

use gfx::Slice;
use gfx::shade::ToUniform;
use gfx::handle::Buffer;
use gfx::traits::FactoryExt;
use gfx::pso::PipelineState;
use euclid::TypedTransform2D;

use types::*;
use graphics_defs::*;
use mesh_gen::*;
use delta::*;
use polyomino::*;
use shape::*;

pub struct DrawSpace;
pub struct ScreenSpace;

pub type DrawTransform = TypedTransform2D<f32, UniverseSpace, DrawSpace>;
pub type CameraTransform = TypedTransform2D<f32, DrawSpace, ScreenSpace>;

#[derive(Clone)]
struct CameraState {
    transform: CameraTransform,
}

#[derive(Clone)]
struct TransformTracker {
    delta: Delta,
    transform: DrawTransform,
}

pub trait To2dGlTransform {
    fn to_gl_mat3(&self) -> [[f32; 3]; 3];
}

impl To2dGlTransform for TypedTransform2D<f32, DrawSpace, ScreenSpace> {
    fn to_gl_mat3(&self) -> [[f32; 3]; 3] {
        let [m11, m12, m21, m22, m31, m32] = self.to_row_major_array();

        [[m11, m12, 0.0], [m21, m22, 0.0], [m31, m32, 1.0]]
    }
}

// impl TransformTracker {
//     // TODO: This will have to adjust for movement in progress
//     fn transform_to_child_chunk(parent: &Chunk, child: &Chunk) -> ScreenTransform {
//         //let p = child.position_on(parent);
//         unimplemented!();
//     }

//     pub fn zoom_out(&self, universe: &Universe) -> TransformTracker {
//         let &TransformTracker {
//             shape_id,
//             ref transform,
//         } = self;

//         TransformTracker {
//             shape_id: unimplemented!(), // universe.parent_of(shape_id),
//             transform: unimplemented!(),
//         }
//     }
//     pub fn zoom_in_to(&self, universe: &Universe) -> TransformTracker {
//         unimplemented!();
//     }

//     pub fn transform_for_constituent() -> ScreenTransform {
//         unimplemented!();
//     }
// }

pub struct Director<'a, R: gfx::Resources> {
    pub resolution: [u32; 2],
    shape_pso: PipelineState<R, shape_pipe::Meta>,
    // shape_vertex_buffer:
    // shape_slice: Slice<R>,
    game_state: &'a GameState,
    mesh_store: MeshStore,
    poly_mesh_buffer: Buffer<R, GpuShapeVertex>,
}

impl<'a, R: gfx::Resources> Director<'a, R> {
    pub fn new<F: gfx::traits::Factory<R>>(
        u: &'a GameState,
        factory: &mut F,
        resolution: [u32; 2],
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

        Director {
            shape_pso: shape_pso,
            // shape_vertex_buffer: vertex_buffer,
            // shape_slice: slice,
            resolution: resolution,
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
            // TODO! Reuse mesh for identical polys
            // if result.contains_key(&s.polyomino) {
            //     continue;
            // }

            result.gen_polyomino_mesh(&s.polyomino);
        }

        result
    }

    fn execute_poly_draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        poly: &Polyomino,
        transform: &TypedTransform2D<f32, DrawSpace, ScreenSpace>,
        outline_color: [f32; 3],
        fill_color: [f32; 3],
    ) -> () {
        let fill_slice = self.mesh_store.poly_meshes.gfx_slice_for(PolyMeshId {
            poly: poly.clone(),
            which: PolyMeshType::GridMesh,
        });
        let arr_transform = transform.to_gl_mat3();

        let fill_data = shape_pipe::Data {
            vbuf: self.poly_mesh_buffer.clone(),
            out: target.clone(),
            resolution: [self.resolution[0] as i32, self.resolution[1] as i32],
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
            resolution: [self.resolution[0] as i32, self.resolution[1] as i32],
            transform: arr_transform,
            color: outline_color,
        };

        encoder.draw(&outline_slice, &self.shape_pso, &outline_data);
    }

    fn draw_recurse<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        //        current_transform: Transform,
        depth: u32,
        max_depth: u32,
    ) -> () {
        unimplemented!();
        // self.draw_single_shape(encoder, target, &self.game_state.player_chunk);
    }

    pub fn draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
    ) -> () {
        unimplemented!();
        // self.draw_single_shape(encoder, target, self.game_state.player_chunk);
    }
}
