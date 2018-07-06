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

    fn intended_target(logical_state: &LogicalState) -> TopChunk {
        logical_state.universe.parent_of(&logical_state.player_chunk)
    }

    fn target_transform_for(
        chunk: &TopChunk,
        logical_state: &LogicalState,
        bounds: TypedRect<f32, DrawSpace>,
    ) -> TypedTransform2D<f32, UniverseSpace, DrawSpace> {
        let chunk_bounds = chunk.bounding_box(&logical_state.universe);

        transform::fit_rect_in(
            &chunk_bounds
                .to_f32()
                .inflate(DRAW_PARENT_MARGIN, DRAW_PARENT_MARGIN),
            &bounds,
        )
    }

    pub fn new(resolution: &TypedSize2D<u32, ScreenSpace>, logical_state: &LogicalState) -> CameraState {
        let camera_bounds = TypedRect::new(
            TypedPoint2D::zero(),
            TypedSize2D::new(resolution.width as f32, resolution.height as f32),
        );

        let start_chunk = CameraState::intended_target(&logical_state);
        let transform = CameraState::target_transform_for(&start_chunk, logical_state, camera_bounds);

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

    pub fn do_zoom(&mut self, logical_state: &LogicalState) -> () {
        let new_target = CameraState::intended_target(logical_state);
        self.target_chunk = new_target.clone();
        self.target_neutral_transform =
            CameraState::target_transform_for(&new_target, logical_state, self.camera_bounds);

        self.current_to_target_path = self.current_to_target_path.up_target();
    }

    fn target_to_current_transform(
        &self,
        logical_state: &LogicalState,
    ) -> TypedTransform2D<f32, UniverseSpace, UniverseSpace> {
        self.current_to_target_path
            .as_delta_from(&logical_state.universe, &self.current_chunk)
            .0
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
        let scale_from_neutral = transform::scale(&self
            .current_transform
            .post_mul(&self.current_neutral_transform.inverse().unwrap()));

        if scale_from_neutral > CAMERA_UPPER_NORMALISE_TRIGGER
            || scale_from_neutral < CAMERA_LOWER_NORMALISE_TRIGGER
        {
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
                .pre_mul(&adjustment.invert().to_scale_transform());

            self.current_to_target_path = self.current_to_target_path.drop(1);
        }
    }

    // This should/could rely on the focus of the camera not being the
    // player, but the parent of the player
    pub fn recenter(&mut self, logical_state: &LogicalState) -> () {
        // let old_target_chunk = &self.target_chunk;
        // let new_target_chunk = CameraState::intended_target(logical_state);
        // let new_target_shape = old_target_chunk.common_shape_with(&new_target_chunk);
        //unimplemented!();
    }
}
