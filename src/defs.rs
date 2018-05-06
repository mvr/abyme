extern crate gfx;

use num::Integer;

use euclid::{TypedPoint2D, TypedVector2D};

////////////////////////////////////////
// Types

pub struct UniverseSpace;
pub struct ChildSpace;

pub type UVec = TypedVector2D<i32, UniverseSpace>;
pub type UPoint = TypedPoint2D<i32, UniverseSpace>;
pub type ChildVec = TypedVector2D<i32, ChildSpace>;
pub type ChildPoint = TypedPoint2D<i32, ChildSpace>;

// The intention is that the camera will be pointed at the origin of DrawSpace
pub struct DrawSpace;
pub struct ScreenSpace;
pub struct GLSpace;


////////////////////////////////////////
// Gameplay

pub const ZOOM_SCALE: u32 = 2;

////////////////////////////////////////
// Drawing

pub const POLY_PERIMETER_GRID_THICKNESS: f32 = 0.05;
pub const POLY_INTERIOR_GRID_THICKNESS: f32 = 0.02;

////////////////////////////////////////
// Graphics

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
