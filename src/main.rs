use board::Board;

mod board;
mod consts;
mod moves;

fn main() {
    let board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    println!("Board initialized from FEN:");
    println!("{}", board);

    let moves = board.possible_moves();
    for mv in moves {
        println!("Possible move: {}", mv);
    }
}
