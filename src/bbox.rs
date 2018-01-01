use types::*;

struct BBox {
    ll: FVec2,
    ur: FVec2,
}

impl BBox {
    fn max_dim(&self) -> f32 {
        (self.ur.x - self.ll.x).max(self.ur.y - self.ll.y)
    }
}
