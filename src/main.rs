use engine::Engine;

mod board;
mod consts;
mod engine;
mod eval;
mod moves;

fn main() {
    Engine::new().start();
}
