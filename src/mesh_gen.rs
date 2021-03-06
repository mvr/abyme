extern crate lyon;

use euclid::Rect;
use lyon::math::vector;
use lyon::path::builder::*;
use lyon::path::default::Path;
use lyon::tessellation::geometry_builder::GeometryBuilder;
use lyon::tessellation::{
    FillOptions, FillVertex, LineCap, StrokeOptions, StrokeTessellator, StrokeVertex,
};

use defs::*;
use polyomino::*;

use mesh_collector::*;

// TODO: In the future we will probably want art more complicated than
// rectangles. For this we should load SVGs and use the `pathfinder`
// crate to render them nicely at any zoom level.

#[derive(Debug, Clone, Copy)]
pub enum GridVertexType {
    Interior = 0,
    Perimeter,
}

#[derive(Debug, Clone, Copy)]
pub enum FillVertexType {
    FillVertex = 0,
}

#[derive(Debug, Clone, Copy)]
struct GridExtraData {
    vertex_type: GridVertexType,
}

impl VertexConstructor<StrokeVertex, GpuShapeVertex> for GridExtraData {
    fn new_vertex(&mut self, vertex: StrokeVertex) -> GpuShapeVertex {
        debug_assert!(!vertex.position.x.is_nan());
        debug_assert!(!vertex.position.y.is_nan());
        debug_assert!(!vertex.normal.x.is_nan());
        debug_assert!(!vertex.normal.y.is_nan());
        debug_assert!(!vertex.advancement.is_nan());

        let thickness = match self.vertex_type {
            GridVertexType::Interior => POLY_INTERIOR_GRID_THICKNESS,
            GridVertexType::Perimeter => POLY_PERIMETER_GRID_THICKNESS,
        };

        //        let adjusted_pos = vertex.position + (vertex.normal * thickness);
        let adjusted_pos = vertex.position;

        GpuShapeVertex {
            position: adjusted_pos.to_array(),
            //            normal: vertex.normal.to_array(),
            vertex_type: self.vertex_type as u32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FillExtraData {
    vertex_type: FillVertexType,
}

impl VertexConstructor<FillVertex, GpuShapeVertex> for FillExtraData {
    fn new_vertex(&mut self, vertex: FillVertex) -> GpuShapeVertex {
        debug_assert!(!vertex.position.x.is_nan());
        debug_assert!(!vertex.position.y.is_nan());
        debug_assert!(!vertex.normal.x.is_nan());
        debug_assert!(!vertex.normal.y.is_nan());
        GpuShapeVertex {
            position: vertex.position.to_array(),
            //            normal: vertex.normal.to_array(),
            vertex_type: self.vertex_type as u32,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum PolyMeshType {
    GridMesh,
    FillMesh,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct PolyMeshId {
    pub poly: Polyomino,
    pub which: PolyMeshType,
}

#[derive(Debug)]
pub struct MeshStore {
    pub poly_meshes: MeshCollection<PolyMeshId, GpuShapeVertex>,
}

impl MeshStore {
    const TOLERANCE: f32 = 0.002; // TODO: what should this be?

    pub fn new() -> MeshStore {
        MeshStore {
            poly_meshes: MeshCollection::new(),
        }
    }

    fn gen_border_path(p: &Polyomino, sort: GridSegmentType) -> Path {
        let mut path_builder = Path::builder();
        for GridSegment(start, end, t) in p.segments() {
            if t != sort {
                continue;
            }
            path_builder.move_to(start.to_untyped().to_f32());
            path_builder.line_to(end.to_untyped().to_f32());
        }
        path_builder.build()
    }

    pub fn gen_polyomino_mesh(&mut self, poly: &Polyomino) {
        let interior_grid_path = MeshStore::gen_border_path(poly, GridSegmentType::Internal);
        let mut grid_adder: MeshAdder<
            PolyMeshId,
            GpuShapeVertex,
            StrokeVertex,
            GridExtraData,
        > = MeshAdder::new(
            &mut self.poly_meshes,
            PolyMeshId {
                poly: poly.clone(),
                which: PolyMeshType::GridMesh,
            },
            GridExtraData {
                vertex_type: GridVertexType::Interior,
            },
        );

        StrokeTessellator::new().tessellate_path(
            interior_grid_path.path_iter(),
            &StrokeOptions::tolerance(MeshStore::TOLERANCE)
                .with_line_width(POLY_INTERIOR_GRID_THICKNESS)
                .with_line_cap(LineCap::Round),
            &mut grid_adder,
        );

        let perimeter_grid_path = MeshStore::gen_border_path(poly, GridSegmentType::Perimeter);
        grid_adder.update_ctor(GridExtraData {
            vertex_type: GridVertexType::Perimeter,
        });

        StrokeTessellator::new().tessellate_path(
            perimeter_grid_path.path_iter(),
            &StrokeOptions::tolerance(MeshStore::TOLERANCE)
                .with_line_width(POLY_PERIMETER_GRID_THICKNESS)
                .with_line_cap(LineCap::Round),
            &mut grid_adder,
        );
        grid_adder.finalise_add();

        let fill_options = FillOptions::default();
        let mut fill_adder = MeshAdder::new(
            &mut self.poly_meshes,
            PolyMeshId {
                poly: poly.clone(),
                which: PolyMeshType::FillMesh,
            },
            FillExtraData {
                vertex_type: FillVertexType::FillVertex,
            },
        );
        fill_adder.begin_geometry();
        for p in poly.square_rects() {
            MeshStore::ccw_fill_rectangle(&p.to_untyped().to_f32(), &fill_options, &mut fill_adder);
        }

        fill_adder.end_geometry();
        fill_adder.finalise_add();
    }

    // CAUTION!! The Euclid methods bottom_left etc consider y positive going DOWN
    fn ccw_fill_rectangle(
        rect: &Rect<f32>,
        _options: &FillOptions,
        output: &mut GeometryBuilder<FillVertex>,
    ) -> () {
        let a = output.add_vertex(FillVertex {
            position: rect.origin,
            normal: vector(-1.0, -1.0),
        });
        let b = output.add_vertex(FillVertex {
            position: rect.top_right(),
            normal: vector(1.0, -1.0),
        });
        let c = output.add_vertex(FillVertex {
            position: rect.bottom_right(),
            normal: vector(1.0, 1.0),
        });
        let d = output.add_vertex(FillVertex {
            position: rect.bottom_left(),
            normal: vector(-1.0, 1.0),
        });
        output.add_triangle(a, b, c);
        output.add_triangle(a, c, d);
    }

    pub fn contains_poly(&self, poly: &Polyomino) -> bool {
        self.poly_meshes.contains_key(&PolyMeshId {
            poly: poly.clone(),
            which: PolyMeshType::GridMesh,
        })
    }
}
