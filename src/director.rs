extern crate gfx;

use gfx::Slice;
use gfx::handle::Buffer;
use gfx::traits::FactoryExt;
use gfx::pso::PipelineState;

use graphics_defs::*;

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

pub struct Director<R: gfx::Resources> {
    pub shape_pso: PipelineState<R, shape_pipe::Meta>,
    pub shape_vertex_buffer: Buffer<R, ShapeVertex>,
    pub shape_slice: Slice<R>,
}

impl<R: gfx::Resources> Director<R> {
    pub fn new<F: gfx::traits::Factory<R>>(mut factory: F) -> Director<R> {
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
        let (vertex_buffer, slice) = factory.create_vertex_buffer_with_slice(SQUARE, INDICES);

        Director {
            shape_pso: shape_pso,
            shape_vertex_buffer: vertex_buffer,
            shape_slice: slice,
        }
    }

    pub fn draw<C: gfx::CommandBuffer<R>>(
        &self,
        encoder: &mut gfx::Encoder<R, C>,
        target: &gfx::handle::RenderTargetView<R, ColorFormat>,
    ) -> () {
        let mut data = shape_pipe::Data {
            vbuf: self.shape_vertex_buffer.clone(),
            out: target.clone(),
        };

        encoder.draw(&self.shape_slice, &self.shape_pso, &data);
    }
}

const SQUARE: &[ShapeVertex] = &[
    ShapeVertex {
        pos: [0.5, -0.5],
        color: WHITE,
    },
    ShapeVertex {
        pos: [-0.5, -0.5],
        color: WHITE,
    },
    ShapeVertex {
        pos: [-0.5, 0.5],
        color: WHITE,
    },
    ShapeVertex {
        pos: [0.5, 0.5],
        color: WHITE,
    },
];

const INDICES: &[u16] = &[0, 1, 2, 2, 3, 0];
