extern crate gfx;

use std::collections::HashMap;

use gfx::Slice;
use gfx::handle::Buffer;
use gfx::traits::FactoryExt;
use gfx::pso::PipelineState;
use cgmath::*;

use graphics_defs::*;
use polyomino::*;
use shape::*;

#[derive(Clone, Copy)]
pub enum VertexType {
    FillVertex = 0,
    OutlineVertex,
}

gfx_defines! {
    vertex ShapeVertex {
        pos: [f32; 2] = "a_Pos",
        vertex_type: u32 = "a_VertexType",
    }

    pipeline shape_pipe {
        vbuf: gfx::VertexBuffer<ShapeVertex> = (),
        fill_color: gfx::Global<[f32; 3]> = "i_FillColor",
        outline_color: gfx::Global<[f32; 3]> = "i_OutlineColor",
        out: gfx::RenderTarget<ColorFormat> = "Target0",
    }
}

// const WHITE: [f32; 3] = [1.0, 1.0, 1.0];

pub struct Director<'a, R: gfx::Resources> {
    shape_pso: PipelineState<R, shape_pipe::Meta>,
    // shape_vertex_buffer:
    // shape_slice: Slice<R>,
    game_state: &'a GameState,
    precomputed_polyomino_meshes: HashMap<Polyomino, (Buffer<R, ShapeVertex>, Slice<R>)>,
}

impl<'a, R: gfx::Resources> Director<'a, R> {
    pub fn new<F: gfx::traits::Factory<R>>(u: &'a GameState, factory: &mut F) -> Director<'a, R> {
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

        let c = Director::build_mesh_cache(u, factory);

        Director {
            shape_pso: shape_pso,
            // shape_vertex_buffer: vertex_buffer,
            // shape_slice: slice,
            game_state: u,
            precomputed_polyomino_meshes: c,
        }
    }

    // TODO: would be best if this was all packed into one VB
    fn build_mesh_cache<F: gfx::traits::Factory<R>>(
        gs: &'a GameState,
        factory: &mut F,
    ) -> HashMap<Polyomino, (Buffer<R, ShapeVertex>, Slice<R>)> {
        let mut result = HashMap::new();

        for (_, s) in &gs.universe.shapes {
            if result.contains_key(&s.polyomino) {
                continue;
            }

            result.insert(
                s.polyomino.clone(),
                Director::build_polyomino_mesh(&s.polyomino, factory),
            );
        }

        result
    }

    fn build_polyomino_mesh<F: gfx::traits::Factory<R>>(
        p: &Polyomino,
        factory: &mut F,
    ) -> (Buffer<R, ShapeVertex>, Slice<R>) {
        let mut vertices = vec![];
        let mut indices = vec![];

        for pos in &p.squares {
            let (ref mut newverts, ref mut newindices) =
                generate_bordered_square(pos.cast(), 0.5, 0.02);

            vertices.append(newverts);
            indices.append(newindices);
        }

        factory.create_vertex_buffer_with_slice(&vertices, &*indices)
    }


    pub fn draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
    ) -> () {
        // for (_, s) in &self.game_state.universe.shapes {
        // }
        let s = &self.game_state.player_chunk();
        let &(ref vb, ref slice) = self.precomputed_polyomino_meshes.get(&s.polyomino).unwrap();

        let data = shape_pipe::Data {
            vbuf: vb.clone(),
            out: target.clone(),
            fill_color: [1.0, 1.0, 1.0],
            outline_color: [0.5, 0.5, 0.5],
        };

        encoder.draw(&slice, &self.shape_pso, &data);
    }
}

// USE MATH COORDINATES
// X RIGHT
// Y UP
// ALWAYS COUNTER-CLOCKWISE
fn quad_indices(ll: u16, lr: u16, ur: u16, ul: u16) -> [u16; 6] {
    [ll, lr, ur, ll, ur, ul]
}

fn generate_quad(
    ll: [f32; 2],
    lr: [f32; 2],
    ur: [f32; 2],
    ul: [f32; 2],
    vt: VertexType,
) -> (Vec<ShapeVertex>, Vec<u16>) {
    let vertices = vec![
        // For fill
        ShapeVertex {
            pos: ll,
            vertex_type: vt as u32
        },
        ShapeVertex {
            pos: lr,
            vertex_type: vt as u32,
        },
        ShapeVertex {
            pos: ur,
            vertex_type: vt as u32,
        },
        ShapeVertex {
            pos: ul,
            vertex_type: vt as u32,
        },
    ];
    let mut indices = vec![];
    indices.extend_from_slice(&quad_indices(0, 1, 2, 3));

    (vertices, indices)
}

fn rect_vertices(ll: [f32; 2], ur: [f32; 2]) -> ([f32; 2], [f32; 2], [f32; 2], [f32; 2]) {
    (ll, [ur[0], ll[1]], ur, [ll[0], ur[1]])
}

fn generate_bordered_square(
    offset: Vector2<f32>,
    size: f32,
    border_width: f32,
) -> (Vec<ShapeVertex>, Vec<u16>) {
    let (ll, lr, ur, ul) = rect_vertices([offset[0], offset[1]], [offset[0] + size, offset[1] + size]);
    let (ill, ilr, iur, iul) = rect_vertices(
            [offset[0] + border_width, offset[1] + border_width],
            [offset[0] + size - border_width, offset[1] + size - border_width],
        );

    use director::VertexType::*;
    let (mut fv, mut fi) = generate_quad(ll, lr, ur, ul, FillVertex);

    let (mut bv, mut bi) = generate_quad(ll, lr, ilr, ill, OutlineVertex); // TODO adjust indices
    let (mut rv, mut ri) = generate_quad(lr, ur, iur, ilr, OutlineVertex);
    let (mut tv, mut ti) = generate_quad(ur, ul, iul, iur, OutlineVertex);
    let (mut lv, mut li) = generate_quad(ul, ll, ill, iul, OutlineVertex);

    let mut vertices = vec![];

    vertices.append(&mut fv);
    vertices.append(&mut bv);
    vertices.append(&mut rv);
    vertices.append(&mut tv);
    vertices.append(&mut lv);

    let mut indices = vec![];

    indices.append(&mut fi);
    indices.append(&mut bi.iter().map(|&x| x + 4).collect());
    indices.append(&mut ri.iter().map(|&x| x + 8).collect());
    indices.append(&mut ti.iter().map(|&x| x + 12).collect());
    indices.append(&mut li.iter().map(|&x| x + 16).collect());

    (vertices, indices)
}
