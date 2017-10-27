#[macro_use] extern crate gfx;

extern crate gfx_window_glutin;
extern crate glutin;

use gfx::traits::FactoryExt;
use gfx::Device;
use gfx_window_glutin as gfx_glutin;
use glutin::GlContext;

pub type ColorFormat = gfx::format::Srgba8;
pub type DepthFormat = gfx::format::DepthStencil;

const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

pub fn main() {
    let mut events_loop = glutin::EventsLoop::new();
    let builder = glutin::WindowBuilder::new()
        .with_title("Square Toy".to_string())
        .with_dimensions(800, 800);
    let context = glutin::ContextBuilder::new()
        .with_vsync(true);

    let (window, mut device, mut factory, mut main_color, mut main_depth) =
        gfx_glutin::init::<ColorFormat, DepthFormat>(builder, context, &events_loop);

    let mut encoder: gfx::Encoder<_, _> = factory.create_command_buffer().into();

    let mut running = true;
    while running {
        encoder.clear(&main_color, BLACK);
        encoder.flush(&mut device);
        window.swap_buffers().unwrap();

        events_loop.poll_events(|e| {
            use glutin::WindowEvent::*;
            match e {
                glutin::Event::WindowEvent{window_id: _, event} =>
                match event {
                    KeyboardInput { input: glutin::KeyboardInput { virtual_keycode: Some(glutin::VirtualKeyCode::Escape), ..}, .. }
                    | Closed
                        => running = false,
                    Resized(_, _) => {
                        gfx_glutin::update_views(&window, &mut main_color, &mut main_depth);
                    },
                    _ => (),
                },
                _ => (),
            }
        });


        window.swap_buffers().unwrap();
        device.cleanup();
    }
}
