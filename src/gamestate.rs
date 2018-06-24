use std::time;

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
