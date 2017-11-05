#[macro_use]
extern crate gfx;

extern crate gfx_window_glutin;
extern crate glutin;

// Rust's import system feels so janky, why do I have to import this
// here for it to be available in polyomino.rs?
extern crate cgmath;

use gfx::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::GlContext;

mod graphics_defs;
mod director;
mod polyomino;
mod shape;
use graphics_defs::*;
use director::*;
use shape::*;

const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

pub fn main() {
    let mut events_loop = glutin::EventsLoop::new();
    let builder = glutin::WindowBuilder::new()
        .with_title("Abyme".to_string())
        .with_dimensions(800, 800);
    let context = glutin::ContextBuilder::new().with_vsync(true);

    let (window, mut device, mut factory, mut main_color_view, mut main_depth) =
        gfx_glutin::init::<ColorFormat, DepthFormat>(builder, context, &events_loop);
    let mut encoder: gfx::Encoder<_, _> = factory.create_command_buffer().into();


    let game_state = GameState::new();
    let director: Director<_> = Director::new(game_state, factory);

    let mut running = true;
    while running {
        encoder.clear(&main_color_view, BLACK);
        director.draw(&mut encoder, &main_color_view);
        encoder.flush(&mut device);
        window.swap_buffers().unwrap();

        events_loop.poll_events(|e| {
            use glutin::WindowEvent::*;
            if let glutin::Event::WindowEvent { event, .. } = e {
                match event {
                    KeyboardInput {
                        input: glutin::KeyboardInput {
                            virtual_keycode: Some(glutin::VirtualKeyCode::Escape), ..
                        },
                        ..
                    } |
                    Closed => running = false,
                    Resized(_, _) => {
                        gfx_glutin::update_views(&window, &mut main_color_view, &mut main_depth);
                    }
                    _ => (),
                }
            }
        });

        device.cleanup();
    }
}
