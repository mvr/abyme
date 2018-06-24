use std::time;

use euclid::{TypedPoint2D, TypedRect, TypedSize2D, TypedTransform2D};

use defs::*;
use math;
use math::*;
use shape::*;
use camera::CameraState;

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
            let newprogress = progress + math::time::duration_to_secs(time_delta);
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
        return self.clone();
    }

    pub fn move_complete(&self) -> bool {
        match *self {
            MoveState::MoveComplete { .. } => true,
            _ => false,
        }
    }
}

pub struct GameState {
    pub logical_state: LogicalState,
    pub camera_state: CameraState,
    pub move_state: MoveState,
}

impl GameState {
    pub fn new(resolution: TypedSize2D<u32, ScreenSpace>) -> GameState {
        let logical_state = LogicalState::minimal();
        let camera_state = CameraState::new(&resolution, &logical_state);
        GameState {
            logical_state,
            camera_state,
            move_state:  MoveState::None,
        }
    }

    pub fn do_zoom(&mut self) -> () {
        self.logical_state.do_zoom();
        self.camera_state.do_zoom(&self.logical_state);
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
        self.logical_state.do_move(d);
        self.camera_state.recenter(&self.logical_state.player_chunk);
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
}
