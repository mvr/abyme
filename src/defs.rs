extern crate gfx;

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

// WARNING: if this is changed then places where I have used >>
// instead of /scale will break
pub const ZOOM_SCALE: u32 = 2;

////////////////////////////////////////
// Drawing

pub const DRAW_DISTANCE_UP: u32 = 1;//3;
pub const DRAW_DISTANCE_DOWN: u32 = 1;//8;

pub const POLY_PERIMETER_GRID_THICKNESS: f32 = 0.05;
pub const POLY_INTERIOR_GRID_THICKNESS: f32 = 0.02;

pub const FRACTIONAL_DELTA_SCALE: u32 = 6;

pub const CAMERA_LERP_SPEED: f32 = 5.0;

pub const CAMERA_UPPER_NORMALISE_TRIGGER: f32 = 3.0;
pub const CAMERA_LOWER_NORMALISE_TRIGGER: f32 = 0.3;

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
