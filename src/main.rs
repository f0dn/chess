use engine::Engine;

mod board;
mod consts;
mod engine;
mod eval;
mod moves;
mod zobrist;

fn main() {
    Engine::new().start();
}
