use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;

use lyon::tessellation::geometry_builder::*;
use gfx::{Slice, IndexBuffer, Resources};

pub struct MeshCollection<IdType, VertexType> {
    pub vertices: Vec<VertexType>,
    pub all_indices: Vec<Index>,
    pub mesh_indices: HashMap<IdType, (Index, Index)>,
}

impl<IdType, VertexType> MeshCollection<IdType, VertexType>
where IdType: Eq + Hash
{
    pub fn new() -> MeshCollection<IdType, VertexType> {
        MeshCollection {
            vertices: vec![],
            all_indices: vec![],
            mesh_indices: hashmap![],
        }
    }

    pub fn gfx_slice_for<R: Resources>(&self, id: IdType) -> Slice<R> {
        let (start, end) = self.mesh_indices[&id];
        Slice {
            start: start as u32,
            end: end as u32,
            base_vertex: 0,
            instances: None,
            buffer: IndexBuffer::Auto,
        }
    }
}

// TODO: This could avoid adding redundant vertices

pub struct MeshAdder<'l, IdType: 'l, VertexType: 'l, Input, Ctor> {
    buffers: &'l mut MeshCollection<IdType, VertexType>,
    new_id: IdType,
    vertex_offset: Index,
    index_offset: Index,
    vertex_constructor: Ctor,
    _marker: PhantomData<Input>,
}

impl<'l, IdType, VertexType: 'l, Input, Ctor> MeshAdder<'l, IdType, VertexType, Input, Ctor>
where
    IdType: Eq + Hash,
{
    pub fn new(
        buffers: &'l mut MeshCollection<IdType, VertexType>,
        id: IdType,
        ctor: Ctor,
    ) -> MeshAdder<'l, IdType, VertexType, Input, Ctor> {
        let vertex_offset = buffers.vertices.len() as Index;
        let index_offset = buffers.all_indices.len() as Index;
        MeshAdder {
            buffers: buffers,
            new_id: id,
            vertex_offset: vertex_offset,
            index_offset: index_offset,
            vertex_constructor: ctor,
            _marker: PhantomData,
        }
    }

    pub fn update_ctor(&mut self, ctor: Ctor) {
        self.vertex_constructor = ctor;
    }

    pub fn finalise_add(self) -> () {
        self.buffers.mesh_indices.insert(self.new_id, (self.index_offset, self.buffers.all_indices.len() as Index));
    }

    pub fn buffers<'a, 'b: 'a>(&'b self) -> &'a MeshCollection<IdType, VertexType> {
        self.buffers
    }
}

pub trait VertexConstructor<Input, VertexType> {
    fn new_vertex(&mut self, input: Input) -> VertexType;
}

impl<'l, IdType, VertexType, Input, Ctor> GeometryBuilder<Input>
    for MeshAdder<'l, IdType, VertexType, Input, Ctor>
where
    VertexType: 'l + Clone,
    Ctor: VertexConstructor<Input, VertexType>,
{
    fn begin_geometry(&mut self) {
        self.vertex_offset = self.buffers.vertices.len() as Index;
        self.index_offset = self.buffers.all_indices.len() as Index;
    }

    fn end_geometry(&mut self) -> Count {
        return Count {
            vertices: self.buffers.vertices.len() as u32 - self.vertex_offset as u32,
            indices: self.buffers.all_indices.len() as u32 - self.index_offset as u32,
        };
    }

    fn add_vertex(&mut self, v: Input) -> VertexId {
        self.buffers
            .vertices
            .push(self.vertex_constructor.new_vertex(v));
        return VertexId(self.buffers.vertices.len() as Index - 1 - self.vertex_offset);
    }

    fn add_triangle(&mut self, a: VertexId, b: VertexId, c: VertexId) {
        self.buffers
            .all_indices
            .push(a.offset() + self.vertex_offset);
        self.buffers
            .all_indices
            .push(b.offset() + self.vertex_offset);
        self.buffers
            .all_indices
            .push(c.offset() + self.vertex_offset);
    }

    fn abort_geometry(&mut self) {
        self.buffers.vertices.truncate(self.vertex_offset as usize);
        self.buffers
            .all_indices
            .truncate(self.index_offset as usize);
    }
}
