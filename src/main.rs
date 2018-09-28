#![feature(slice_patterns)]
#![feature(custom_attribute)]
#![feature(nll)]
#![feature(stmt_expr_attributes)]
#![cfg_attr(test, feature(plugin))]
// #![cfg_attr(test, plugin(quickcheck_macros))]

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

// #[cfg(test)]
// #[macro_use]
// extern crate quickcheck;

mod defs;
mod math;

mod mesh_collector;
mod mesh_gen;

mod delta;
mod draw_delta;

mod camera;
mod gamestate;
mod director;
mod polyomino;
mod shape;

mod load_universe;

use std::env;

use gfx_core::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::GlContext;

use euclid::TypedSize2D;

use defs::*;
use director::*;
use math::*;
use shape::*;

const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

fn universe_filename() -> String {
    let first_argument = std::env::args().nth(1);

    match first_argument {
        Some(name) => name,
        None => "universe/minimal.toml".to_string()
    }
}

pub fn main() {
    let mut events_loop = glutin::EventsLoop::new();
    let resolution = TypedSize2D::new(1024, 768);
    let builder = glutin::WindowBuilder::new()
        .with_title("Abyme".to_string())
        .with_dimensions(resolution.width, resolution.height);
    let context = glutin::ContextBuilder::new().with_vsync(true);

    let (window, mut device, mut factory, mut main_color_view, mut main_depth) =
        gfx_glutin::init::<ColorFormat, DepthFormat>(builder, context, &events_loop);
    let mut encoder: gfx::Encoder<_, _> = factory.create_command_buffer().into();


    let logical_state = load_universe::load_universe(universe_filename());

    let mut director: Director<_> = Director::new(&mut factory, logical_state, resolution);

    let mut last_time = std::time::Instant::now();

    let mut running = true;
    while running {
        events_loop.poll_events(|e| {
            use glutin::WindowEvent::*;
            if let glutin::Event::WindowEvent { event, .. } = e {
                match event {
                    CloseRequested => {
                        running = false;
                    }

                    Resized(w, h) => {
                        gfx_glutin::update_views(&window, &mut main_color_view, &mut main_depth);
                        director.resolution = TypedSize2D::new(w, h);
                        // TODO: recalculate camera
                    }

                    KeyboardInput {
                        input:
                            glutin::KeyboardInput {
                                virtual_keycode: Some(key),
                                state: glutin::ElementState::Pressed,
                                ..
                            },
                        ..
                    } => {
                        use glutin::VirtualKeyCode::*;
                        match key {
                            Escape => running = false,
                            Space => director.game_state.do_zoom(),
                            Up => director.game_state.try_start_move(Direction::Up),
                            Down => director.game_state.try_start_move(Direction::Down),
                            Left => director.game_state.try_start_move(Direction::Left),
                            Right => director.game_state.try_start_move(Direction::Right),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        });

        encoder.clear(&main_color_view, BLACK);
        director.draw(&mut encoder, &main_color_view);
        encoder.flush(&mut device);
        window.swap_buffers().unwrap();
        device.cleanup();

        let delta = std::time::Instant::now().duration_since(last_time);
        last_time = std::time::Instant::now();

        director.game_state.update(delta);
    }
}
