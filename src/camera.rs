use std::ops::{Add, Div};
use std::time;

use euclid::{TypedPoint2D, TypedRect, TypedSize2D, TypedTransform2D};

use defs::*;
use math;
use math::*;
use shape::*;

// TODO: Do I need a custom AATransform that manages precision
// somehow?

// TODO: think about how movement will work: this is tricky. When a
// chunk is broken in to pieces, we need to make sure our
// current_shape and target_shape are switched to being ones that are
// in the correct chunk

#[derive(Debug)]
pub struct CameraState {
    pub camera_bounds: TypedRect<f32, DrawSpace>,

    pub current_chunk: TopChunk,
    pub current_neutral_transform: TypedTransform2D<f32, UniverseSpace, DrawSpace>,
    pub current_transform: TypedTransform2D<f32, UniverseSpace, DrawSpace>,

    pub target_chunk: TopChunk, // This should always be the parent chunk of the player chunk
    pub target_neutral_transform: TypedTransform2D<f32, UniverseSpace, DrawSpace>,

    pub current_to_target_path: MonotonePath, // This should stay in sync with the above...
}

impl CameraState {
    fn center<T, U>(rect: &TypedRect<T, U>) -> TypedPoint2D<T, U>
    where
        T: Copy + Add<T, Output = T> + Div<T, Output = T> + From<f32>,
    {
        TypedPoint2D::new(
            rect.origin.x + rect.size.width / T::from(2.0),
            rect.origin.y + rect.size.height / T::from(2.0),
        )
    }

    fn intended_target_chunk(logical_state: &LogicalState) -> TopChunk {
        logical_state
            .universe
            .parent_of(&logical_state.player_chunk)
    }

    fn target_transform_for(
        chunk: &TopChunk,
        logical_state: &LogicalState,
        bounds: TypedRect<f32, DrawSpace>,
    ) -> TypedTransform2D<f32, UniverseSpace, DrawSpace> {
        let chunk_bounds = logical_state
            .universe
            .region_of_chunk(chunk)
            .bounding_box(&logical_state.universe);

        transform::fit_rect_in(
            &chunk_bounds
                .to_f32()
                .inflate(DRAW_PARENT_MARGIN, DRAW_PARENT_MARGIN),
            &bounds,
        )
    }

    pub fn new(
        resolution: &TypedSize2D<u32, ScreenSpace>,
        logical_state: &LogicalState,
    ) -> CameraState {
        let camera_bounds = TypedRect::new(
            TypedPoint2D::zero(),
            TypedSize2D::new(resolution.width as f32, resolution.height as f32),
        );

        let start_chunk = CameraState::intended_target_chunk(&logical_state);
        let transform =
            CameraState::target_transform_for(&start_chunk, logical_state, camera_bounds);

        CameraState {
            camera_bounds: camera_bounds,

            current_chunk: start_chunk.clone(),
            current_transform: transform,
            current_neutral_transform: transform,

            target_chunk: start_chunk.clone(),
            target_neutral_transform: transform,

            current_to_target_path: MonotonePath::Zero,
        }
    }

    pub fn do_zoom_in(&mut self, logical_state: &LogicalState) -> () {
        // At this point, the logical state has already been zoomed.

        self.recenter(logical_state);

        let new_target = CameraState::intended_target_chunk(logical_state);
        self.target_chunk = new_target.clone();
        self.target_neutral_transform =
            CameraState::target_transform_for(&new_target, logical_state, self.camera_bounds);

        self.current_to_target_path = self
            .current_to_target_path
            .down_target_to(new_target.origin_id);
    }

    pub fn do_zoom_out(&mut self, logical_state: &LogicalState) -> () {
        self.recenter(logical_state);

        let new_target = CameraState::intended_target_chunk(logical_state);
        self.target_chunk = new_target.clone();
        self.target_neutral_transform =
            CameraState::target_transform_for(&new_target, logical_state, self.camera_bounds);

        self.current_to_target_path = self
            .current_to_target_path
            .up_target();
    }

    pub fn scale_from_neutral(&self) -> f32 {
        transform::scale(
            &self
                .current_transform
                .post_mul(&self.current_neutral_transform.inverse().unwrap()),
        )
    }

    pub fn current_visual_level(&self) -> f32 {
        self.scale_from_neutral().log(ZOOM_SCALE as f32)
    }

    fn target_to_current_transform(
        &self,
        logical_state: &LogicalState,
    ) -> TypedTransform2D<f32, UniverseSpace, UniverseSpace> {
        self.current_to_target_path
            .as_delta_from(&logical_state.universe, &self.current_chunk)
            .0
            .invert()
            .to_scale_transform()
    }

    fn true_target_transform(
        &self,
        logical_state: &LogicalState,
    ) -> TypedTransform2D<f32, UniverseSpace, DrawSpace> {
        self.target_to_current_transform(logical_state)
            .post_mul(&self.target_neutral_transform)
    }

    pub fn update(&mut self, logical_state: &LogicalState, time_delta: time::Duration) -> () {
        // TODO: This is currently busted but somehow I get away with it?

        self.current_transform = transform::lerp(
            &self.current_transform,
            &self.true_target_transform(logical_state),
            1.0 - (-math::time::duration_to_secs(time_delta) * CAMERA_LERP_SPEED).exp(),
        );

        self.normalise(logical_state);
    }

    fn normalise(&mut self, logical_state: &LogicalState) -> () {
        let scale_from_neutral = self.scale_from_neutral();

        // MUST TODO: This will break if the individual chunks get too
        // big, need to scale by the size of the target/current chunk?
        // println!("{:#?}", scale_from_neutral);

        let need_normalisation = (scale_from_neutral > CAMERA_UPPER_NORMALISE_TRIGGER
            && self.current_to_target_path.zdelta() < 0)
            || (scale_from_neutral < CAMERA_LOWER_NORMALISE_TRIGGER
                && self.current_to_target_path.zdelta() > 0);

        if need_normalisation {
            let (adjustment, new_current_chunk) = self
                .current_to_target_path
                .take(1)
                .as_delta_from(&logical_state.universe, &self.current_chunk);

            self.current_neutral_transform = CameraState::target_transform_for(
                &new_current_chunk,
                logical_state,
                self.camera_bounds,
            );
            self.current_chunk = new_current_chunk;

            self.current_transform = self
                .current_transform
                .pre_mul(&adjustment.to_scale_transform());

            self.current_to_target_path = self.current_to_target_path.drop(1);
        }
    }

    // We need to pick the chunk in the region that actually contains
    // the player
    pub fn recenter(&mut self, logical_state: &LogicalState) -> () {
        let old_current_chunk = &self.current_chunk;
        let region = logical_state.universe.region_of_chunk(old_current_chunk);

        let mut new_current_chunk_shape_id = logical_state.player_chunk.origin_id;
        while !region
            .top_shape_ids
            .contains_key(&new_current_chunk_shape_id)
        {
            new_current_chunk_shape_id = *logical_state.universe.shapes[&new_current_chunk_shape_id]
                .parent_ids
                .keys()
                .min()
                .unwrap();
        }
        let new_current_chunk: TopChunk = logical_state
            .universe
            .explore(new_current_chunk_shape_id)
            .into();

        let adjustment = region.top_shape_ids[&new_current_chunk.origin_id]
            - region.top_shape_ids[&old_current_chunk.origin_id];

        self.current_transform = self.current_transform.pre_translate(adjustment.to_f32());
        self.current_neutral_transform = CameraState::target_transform_for(
            &new_current_chunk,
            logical_state,
            self.camera_bounds,
        );
        self.current_chunk = new_current_chunk;

        let new_target = CameraState::intended_target_chunk(logical_state);
        self.target_chunk = new_target.clone();
        self.target_neutral_transform =
            CameraState::target_transform_for(&new_target, logical_state, self.camera_bounds);
    }
}
