extern crate gfx;

use std::collections::HashMap;

use euclid::{TypedPoint2D, TypedRect, TypedSize2D, TypedTransform2D};
use gfx::handle::Buffer;
use gfx::pso::PipelineState;
use gfx::traits::FactoryExt;
use gfx::IndexBuffer;

use defs::*;
use delta::Delta;
use gamestate::*;
use math::*;
use mesh_gen::*;
use polyomino::*;
use shape::*;

// TODO: this works under the assumption that the shapes being drawn
// on each level are all distinct. (which is true currently)
#[derive(Clone, Debug, PartialEq)]
struct LevelTracker {
    level: i32,
    visual_level: f32,
    transforms: HashMap<ShapeId, Delta>,
}

impl LevelTracker {
    pub fn from_gamestate(gs: &GameState) -> LevelTracker {
        let chunk = &gs.camera_state.current_chunk;

        let mut result = hashmap![];

        for (shape_id, uvec) in &chunk.top_shape_ids {
            result.insert(*shape_id, Delta::from(*uvec));
        }

        LevelTracker {
            level: 0,
            visual_level: gs.camera_state.current_visual_level(),
            transforms: result,
        }
    }

    pub fn go_up(&self, gamestate: &GameState) -> LevelTracker {
        let mut result: HashMap<ShapeId, Delta> = hashmap![];
        let universe = &gamestate.logical_state.universe;

        for (shape_id, delta) in &self.transforms {
            let shape = &universe.shapes[shape_id];

            for parent in universe.parents_of(shape) {
                if result.contains_key(&parent.id) {
                    continue;
                }

                result.insert(
                    parent.id,
                    delta
                        .revert(&gamestate.visual_delta_to_child(parent, shape))
                        .normalize(),
                );
            }
        }

        LevelTracker {
            level: self.level + 1,
            visual_level: self.visual_level + 1.0,
            transforms: result,
        }
    }

    pub fn go_down(&self, gamestate: &GameState) -> LevelTracker {
        let mut result: HashMap<ShapeId, Delta> = hashmap![];
        let universe = &gamestate.logical_state.universe;

        for (shape_id, delta) in &self.transforms {
            let shape = &universe.shapes[shape_id];

            for child in universe.children_of(shape) {
                if result.contains_key(&child.id) {
                    continue;
                }


                result.insert(
                    child.id,
                    delta
                        .append(&gamestate.visual_delta_to_child(shape, child))
                        .normalize(),
                );
            }
        }

        LevelTracker {
            level: self.level - 1,
            visual_level: self.visual_level - 1.0,
            transforms: result,
        }
    }

    pub fn filter_nonvisible(&mut self) -> () {
        // TODO: do this
    }
}

pub struct Director<R: gfx::Resources> {
    pub resolution: TypedSize2D<u32, ScreenSpace>,

    draw_space_to_gl: TypedTransform2D<f32, DrawSpace, GLSpace>,

    mesh_store: MeshStore,
    poly_mesh_buffer: Buffer<R, GpuShapeVertex>,
    poly_mesh_index_buffer: IndexBuffer<R>,
    shape_pso: PipelineState<R, shape_pipe::Meta>,

    pub game_state: GameState,
}

impl<R: gfx::Resources> Director<R> {
    pub fn new<F: gfx::traits::Factory<R>>(
        factory: &mut F,
        logical_state: LogicalState,
        resolution: TypedSize2D<u32, ScreenSpace>,
    ) -> Director<R> {
        let game_state = GameState::new(logical_state, resolution);

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

        let store = Director::build_mesh_cache(&game_state.logical_state, factory);
        let pmb = factory.create_vertex_buffer(&store.poly_meshes.vertices);
        let pmib = factory.create_index_buffer(&store.poly_meshes.all_indices[..]);

        let ndc_bounds = TypedRect::new(TypedPoint2D::new(-1.0, -1.0), TypedSize2D::new(2.0, 2.0));
        let draw_space_to_gl =
            transform::rect_to_rect(&game_state.camera_state.camera_bounds, &ndc_bounds);

        Director {
            shape_pso,
            resolution,
            draw_space_to_gl,

            mesh_store: store,
            poly_mesh_buffer: pmb,
            poly_mesh_index_buffer: pmib,

            game_state,
        }
    }

    fn build_mesh_cache<F: gfx::traits::Factory<R>>(
        gs: &LogicalState,
        _factory: &mut F,
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

    fn execute_poly_outline_draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        poly: &Polyomino,
        raw_transform: &TypedTransform2D<f32, UniverseSpace, GLSpace>,
        outline_color: [f32; 4],
    ) -> () {
        let arr_transform = (*raw_transform).to_gl_mat3();
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

        encoder.draw(&outline_slice, &self.shape_pso, &outline_data);
    }

    fn execute_poly_fill_draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        poly: &Polyomino,
        raw_transform: &TypedTransform2D<f32, UniverseSpace, GLSpace>,
        fill_color: [f32; 4],
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
    }

    fn fill_fade_amount_for_level(level: f32) -> f32 {
        if level < 0.0 {
            1.0
        } else if 0.0 <= level && level <= 1.0 {
            1.0 - (0.7 * level)
        } else {
            0.3
        }
    }

    fn outline_fade_amount_for_level(level: f32) -> f32 {
        if level < 0.0 {
            1.0
        } else if 0.0 <= level && level <= 1.0 {
            1.0 - level
        } else {
            0.0
        }
    }

    fn draw_level<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
        level_tracker: &LevelTracker,
        which: PolyMeshType,
    ) -> () {
        let transform_to_gl = self
            .game_state
            .camera_state
            .current_transform
            .post_mul(&self.draw_space_to_gl);

        let fill_fade = <Director<R>>::fill_fade_amount_for_level(level_tracker.visual_level);
        let outline_fade = <Director<R>>::outline_fade_amount_for_level(level_tracker.visual_level);

        for (shape_id, offset) in &level_tracker.transforms {
            let shape = &self.game_state.logical_state.universe.shapes[shape_id];
            let offset_transform = offset.to_scale_transform();

            let faded_fill = [
                shape.fill_color[0] * fill_fade,
                shape.fill_color[1] * fill_fade,
                shape.fill_color[2] * fill_fade,
                1.0,
            ];

            let faded_outline = [
                shape.outline_color[0] * outline_fade,
                shape.outline_color[1] * outline_fade,
                shape.outline_color[2] * outline_fade,
                outline_fade,
            ];

            match which {
                PolyMeshType::GridMesh => self.execute_poly_outline_draw(
                    encoder,
                    target,
                    &shape.polyomino,
                    &offset_transform.post_mul(&transform_to_gl),
                    faded_outline,
                ),
                PolyMeshType::FillMesh => self.execute_poly_fill_draw(
                    encoder,
                    target,
                    &shape.polyomino,
                    &offset_transform.post_mul(&transform_to_gl),
                    faded_fill,
                ),
            }
        }
    }

    pub fn draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
    ) -> () {
        let mut l = LevelTracker::from_gamestate(&self.game_state);

        for _ in 0..DRAW_DISTANCE_UP {
            l = l.go_up(&self.game_state);
            l.filter_nonvisible();
        }

        self.draw_level(encoder, target, &l, PolyMeshType::FillMesh);

        for _ in 0..(DRAW_DISTANCE_UP + DRAW_DISTANCE_DOWN) {
            l = l.go_down(&self.game_state);
            l.filter_nonvisible();
            self.draw_level(encoder, target, &l, PolyMeshType::FillMesh);
            self.draw_level(encoder, target, &l, PolyMeshType::GridMesh);
        }

        // for _ in 0..(DRAW_DISTANCE_UP + DRAW_DISTANCE_DOWN) {
        //     l = l.go_up(&self.game_state);
        //     l.filter_nonvisible();
        //     self.draw_level(encoder, target, &l, PolyMeshType::GridMesh);
        // }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     #[test]
//     fn wonky_updown_1() {
//         let gs = GameState {
//             logical_state: LogicalState {
//                 universe: Universe {
//                     shapes: btreemap! {
//                         ShapeId(
//                             1,
//                         ) => Shape {
//                             id: ShapeId(
//                                 1,
//                             ),
//                             parent_ids: hashmap!{
//                                 ShapeId(
//                                     2,
//                                 ) => TypedPoint2D::new(1,0),
//                                 ShapeId(
//                                     3,
//                                 )=> TypedPoint2D::new(-1,0),
//                             },
//                             polyomino: Polyomino {
//                                 squares: vec![
//                                     Point2D::new(0,0),
//                                     Point2D::new(1,0),
//                                 ],
//                             },
//                             fill_color: [
//                                 0.5,
//                                 0.5,
//                                 1.0,
//                             ],
//                             outline_color: [
//                                 0.0,
//                                 0.0,
//                                 0.0,
//                             ],
//                         },
//                         ShapeId(
//                             2,
//                         ) => Shape {
//                             id: ShapeId(
//                                 2,
//                             ),
//                             parent_ids: hashmap!{
//                                 ShapeId(
//                                     1,
//                                 ) => TypedPoint2D::new(0,0),
//                             },
//                             polyomino: Polyomino {
//                                 squares: vec![
//                                     Point2D::new(0,0),
//                                 ],
//                             },
//                             fill_color: [
//                                 1.0,
//                                 0.5,
//                                 0.5,
//                             ],
//                             outline_color: [
//                                 0.0,
//                                 0.0,
//                                 0.0,
//                             ],
//                         },
//                         ShapeId(
//                             3,
//                         ) => Shape {
//                             id: ShapeId(
//                                 3,
//                             ),
//                             parent_ids: hashmap!{
//                                 ShapeId(
//                                     1,
//                                 ) => TypedPoint2D::new(1,0),
//                             },
//                             polyomino: Polyomino {
//                                 squares: vec![
//                                     Point2D::new(0,0),
//                                 ],
//                             },
//                             fill_color: [
//                                 1.0,
//                                 0.5,
//                                 0.5,
//                             ],
//                             outline_color: [
//                                 0.0,
//                                 0.0,
//                                 0.0,
//                             ],
//                         },
//                     },
//                 },
//                 player_chunk: TopChunk {
//                     origin_id: ShapeId(1),
//                     top_shape_ids: btreemap! {
//                         ShapeId(
//                             1,
//                         ) => TypedVector2D::new(0,0),
//                     },
//                 },
//             },
//             camera_state: CameraState {
//                 camera_bounds: TypedRect::new(
//                     TypedPoint2D::new(0.0, 0.0),
//                     TypedSize2D::new(0.0, 0.0),
//                 ),
//                 current_chunk: TopChunk {
//                     origin_id: ShapeId(2),
//                     top_shape_ids: btreemap! {
//                         ShapeId(
//                             2,
//                         ) => TypedVector2D::new(0,0),
//                     },
//                 },
//                 current_neutral_transform: TypedTransform2D::identity(),
//                 current_transform: TypedTransform2D::identity(),
//                 target_chunk: TopChunk {
//                     origin_id: ShapeId(2),
//                     top_shape_ids: btreemap! {
//                         ShapeId(
//                             2,
//                         ) => TypedVector2D::new(0,0),
//                     },
//                 },
//                 target_neutral_transform: TypedTransform2D::identity(),
//                 current_to_target_path: Zero,
//             },
//             move_state: Moving {
//                 chunk: TopChunk {
//                     origin_id: ShapeId(1),
//                     top_shape_ids: btreemap! {
//                         ShapeId(
//                             1,
//                         ) => TypedVector2D::new(0,0),
//                     },
//                 },
//                 direction: Left,
//                 progress: 0.054777056,
//             },
//         };
//         let l = LevelTracker {
//             level: 2,
//             visual_level: 0.0,
//             transforms: hashmap! {
//                 ShapeId(3) => Delta {
//                     zdelta: 2,
//                     scale: 1,
//                     coords: TypedVector2D::new(Integer::from(1),Integer::from(0)),
//                 },
//                 ShapeId(2) => Delta {
//                     zdelta: 2,
//                     scale: -9,
//                     coords: TypedVector2D::new(Integer::from(-1023),Integer::from(0)),
//                 },
//             },
//         };

//         assert_eq!(
//             l.go_up(&gs).go_down(&gs).transforms[&ShapeId(2)],
//             l.transforms[&ShapeId(2)]
//         );
//     }
// }
