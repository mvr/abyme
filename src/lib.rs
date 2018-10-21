#![feature(slice_patterns)]
#![feature(custom_attribute)]
#![feature(nll)]
#![feature(stmt_expr_attributes)]
#![cfg_attr(test, feature(plugin))]
extern crate num;
extern crate rug;

extern crate toml;

#[macro_use]
extern crate gfx;
extern crate gfx_core;
extern crate gfx_window_glutin;
extern crate glutin;
extern crate lyon;

extern crate euclid;
#[macro_use]
extern crate maplit;
extern crate itertools;

pub mod defs;
pub mod math;

pub mod mesh_collector;
pub mod mesh_gen;

pub mod delta;
pub mod draw_delta;

pub mod camera;
pub mod gamestate;
pub mod director;
pub mod polyomino;
pub mod shape;

pub mod load_universe;
