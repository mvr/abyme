#![feature(match_default_bindings)]
#![feature(universal_impl_trait)]
#![feature(conservative_impl_trait)]
#![feature(nll)]

extern crate num;

#[macro_use] extern crate gfx;
extern crate gfx_window_glutin;
extern crate glutin;
extern crate lyon;

// Rust's import system feels so janky, why do I have to import this
// here for it to be available in polyomino.rs?
extern crate euclid;
#[macro_use] extern crate maplit;

mod types;
mod delta;
mod graphics_defs;
mod gameplay_constants;
mod mesh_gen;
mod director;
mod polyomino;
mod shape;
