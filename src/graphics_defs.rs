extern crate gfx;

pub type ColorFormat = gfx::format::Srgba8;
pub type DepthFormat = gfx::format::DepthStencil;

gfx_defines! {
    vertex GpuShapeVertex {
        position: [f32; 2] = "a_Pos",
//        normal: [f32; 2] = "a_Normal",
        vertex_type: u32 = "a_VertexType",
    }

    pipeline shape_pipe {
        vbuf: gfx::VertexBuffer<GpuShapeVertex> = (),
//        resolution: gfx::Global<[i32; 2]> = "i_Resolution",
        transform: gfx::Global<[[f32; 3]; 3]> = "i_Transform",
        color: gfx::Global<[f32; 3]> = "i_Color",
        out: gfx::RenderTarget<ColorFormat> = "Target0",
    }
}
