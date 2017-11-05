extern crate gfx;

use gfx::Slice;
use gfx::handle::Buffer;
use gfx::traits::FactoryExt;
use gfx::pso::PipelineState;

use graphics_defs::*;
use shape::*;

gfx_defines! {
    vertex ShapeVertex {
        pos: [f32; 2] = "a_Pos",
        color: [f32; 3] = "a_Color",
    }

    pipeline shape_pipe {
        vbuf: gfx::VertexBuffer<ShapeVertex> = (),
        out: gfx::RenderTarget<ColorFormat> = "Target0",
    }
}

const WHITE: [f32; 3] = [1.0, 1.0, 1.0];

pub struct Director<'a, R: gfx::Resources> {
    shape_pso: PipelineState<R, shape_pipe::Meta>,
    shape_vertex_buffer: Buffer<R, ShapeVertex>,
    shape_slice: Slice<R>,

    game_state: &'a GameState<'a>,
}

impl<'a, R: gfx::Resources> Director<'a, R> {
    pub fn new<F: gfx::traits::Factory<R>>(u: &'a GameState<'a>, mut factory: F) -> Director<'a, R> {
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

        let (vertices, indices) = generate_square(0.5, 0.2, WHITE, [0.3, 0.3, 0.3]);
        let (vertex_buffer, slice) = factory.create_vertex_buffer_with_slice(&vertices, &*indices);

        Director {
            shape_pso: shape_pso,
            shape_vertex_buffer: vertex_buffer,
            shape_slice: slice,

            game_state: u
        }
    }

    pub fn draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
    ) -> () {
        let data = shape_pipe::Data {
            vbuf: self.shape_vertex_buffer.clone(),
            out: target.clone(),
        };

        encoder.draw(&self.shape_slice, &self.shape_pso, &data);
    }
}

// USE MATH COORDINATES
// X RIGHT
// Y UP
// ALWAYS COUNTER-CLOCKWISE
fn rect_vertices(ll: [f32; 2], ur: [f32; 2]) -> ([f32; 2], [f32; 2], [f32; 2], [f32; 2]) {
    (ll, [ur[0], ll[1]], ur, [ll[0], ur[1]])
}
fn rect_indices(ll: u16, lr: u16, ur: u16, ul: u16) -> [u16; 6] {
    [ll, lr, ur, ll, ur, ul]
}

fn generate_square(
    size: f32,
    border_width: f32,
    fill_color: [f32; 3],
    outline_color: [f32; 3],
) -> (Vec<ShapeVertex>, Vec<u16>) {
    let (ul, ur, ll, lr) = rect_vertices([0.0, 0.0], [size, size]);
    let (iul, iur, ill, ilr) = rect_vertices(
        [border_width, border_width],
        [size - border_width, size - border_width],
    );

    let vertices = vec![
        // For fill
        ShapeVertex {
            pos: ll,
            color: fill_color,
        },
        ShapeVertex {
            pos: lr,
            color: fill_color,
        },
        ShapeVertex {
            pos: ur,
            color: fill_color,
        },
        ShapeVertex {
            pos: ul,
            color: fill_color,
        },
    ];
    let mut indices = vec![];
    indices.extend_from_slice(&rect_indices(0, 1, 2, 3));

    (vertices, indices)
}
