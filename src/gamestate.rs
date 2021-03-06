use std::time;

use euclid::{TypedSize2D, TypedVector2D};

use camera::CameraState;
use defs::*;
use delta::*;
use load_universe;
use math;
use math::*;
use shape::*;

#[derive(Clone, Debug)]
pub enum MoveState {
    None,
    Moving {
        chunk: TopChunk,
        direction: Direction,
        progress: f32,
    },
    MoveComplete {
        chunk: TopChunk,
        direction: Direction,
    },
}

impl MoveState {
    pub fn update(&self, time_delta: time::Duration) -> MoveState {
        if let MoveState::Moving {
            progress,
            chunk,
            direction,
        } = self
        {
            let newprogress = progress + math::time::duration_to_secs(time_delta) / MOVE_TIME;
            if newprogress >= 1.0 {
                return MoveState::MoveComplete {
                    chunk: chunk.clone(),
                    direction: *direction,
                };
            } else {
                return MoveState::Moving {
                    chunk: chunk.clone(),
                    direction: *direction,
                    progress: newprogress,
                };
            }
        }
        self.clone()
    }

    pub fn move_complete(&self) -> bool {
        match *self {
            MoveState::MoveComplete { .. } => true,
            _ => false,
        }
    }

    fn ease(t: f32) -> f32 {
        if t < 0.5 {
            4.0 * t * t * t
        } else {
            (t - 1.0) * (2.0 * t - 2.0) * (2.0 * t - 2.0) + 1.0
        }
    }

    pub fn progress_to_displacement<U>(
        progress: f32,
        direction: Direction,
    ) -> TypedVector2D<f32, U> {
        let visual_progress = MoveState::ease(progress);
        direction.to_vect::<i32, U>().to_f32() * visual_progress
    }
}

#[derive(Debug)]
pub struct GameState {
    pub logical_state: LogicalState,
    pub camera_state: CameraState,
    pub move_state: MoveState,
}

impl GameState {
    pub fn new(
        logical_state: LogicalState,
        resolution: TypedSize2D<u32, ScreenSpace>,
    ) -> GameState {
        let camera_state = CameraState::new(&resolution, &logical_state);
        GameState {
            logical_state,
            camera_state,
            move_state: MoveState::None,
        }
    }

    pub fn do_zoom_in(&mut self) -> () {
        self.logical_state.do_zoom_in();
        self.camera_state.do_zoom_in(&self.logical_state);
    }

    pub fn do_zoom_out(&mut self) -> () {
        self.logical_state.do_zoom_out();
        self.camera_state.do_zoom_out(&self.logical_state);
    }

    pub fn try_start_move(&mut self, d: Direction) -> () {
        if let MoveState::Moving { .. } = self.move_state {
            return;
        }

        if self.logical_state.can_move(d) {
            self.move_state = MoveState::Moving {
                chunk: self.logical_state.player_chunk.clone(),
                direction: d,
                progress: 0.0,
            }
        } else {
            // TODO: visual indicator?
        }
    }

    pub fn do_move(&mut self, d: Direction) -> () {
        assert!(self.logical_state.can_move(d));
        self.logical_state.do_move(d);
        self.camera_state.recenter(&self.logical_state);
    }

    pub fn update(&mut self, time_delta: time::Duration) -> () {
        self.camera_state.update(&self.logical_state, time_delta);
        self.move_state = self.move_state.update(time_delta);

        if self.move_state.move_complete() {
            match self.move_state {
                MoveState::MoveComplete { direction, .. } => self.do_move(direction),
                _ => {}
            }
            self.move_state = MoveState::None;
        }
    }

    pub fn visual_delta_to_child(&self, parent: &Shape, child: &Shape) -> Delta {
        match self.move_state {
            MoveState::Moving {
                ref chunk,
                progress,
                direction,
            } => {
                if chunk.top_shape_ids.contains_key(&child.id) {
                    return child.delta_from_parent(&parent).append(&Delta::from(
                        MoveState::progress_to_displacement::<UniverseSpace>(progress, direction),
                    ));
                }
            }
            _ => (),
        }

        parent.delta_to_child(&child)
    }
}
