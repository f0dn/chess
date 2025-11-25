use board::Board;

mod board;
mod consts;
mod moves;

fn main() {
    let board = Board::from_fen("8/4p3/8/8/4R3/8/8/8 w - - 0 1");
    println!("Board initialized from FEN:");
    println!("{}", board);
    let moves = board.sliding_moves(&[7], consts::ROOK);
    for mv in moves {
        println!("{}", mv);
    }
}
