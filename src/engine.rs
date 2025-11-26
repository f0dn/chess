use std::str::SplitWhitespace;

use crate::board::Board;
use crate::board::print_bitboard;
use crate::consts::*;
use crate::moves::*;
use std::fs::OpenOptions;
use std::io::Write;

pub struct Engine {
    board: Board,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            board: Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
        }
    }

    pub fn start(&mut self) {
        while let Some(command) = self.read_command() {
            self.debug(&format!("Received command: {}", command));
            let mut parts = command.split_whitespace();
            match parts.next().unwrap_or("") {
                "uci" => self.handle_uci(),
                "isready" => self.handle_isready(),
                "position" => self.handle_position(parts),
                "go" => self.handle_go(parts),
                "quit" => break,
                _ => (),
            }
        }
    }

    fn debug(&self, msg: &str) {
        let debug_file = "/Users/there/Documents/ChessEngine/debug.txt";

        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(debug_file)
            .unwrap();

        writeln!(file, "{}", msg).unwrap();
    }

    fn read_command(&self) -> Option<String> {
        use std::io::{self, BufRead};

        let stdin = io::stdin();
        let mut iterator = stdin.lock().lines();

        if let Some(Ok(line)) = iterator.next() {
            Some(line)
        } else {
            None
        }
    }

    fn handle_uci(&self) {
        println!("id name trinity");
        println!("id author flint");
        println!("uciok");
    }

    fn handle_isready(&self) {
        println!("readyok");
    }

    fn handle_position(&mut self, mut parts: SplitWhitespace) {
        if let Some(pos_type) = parts.next() {
            match pos_type {
                "startpos" => {
                    self.board =
                        Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
                }
                "fen" => {
                    let fen: String = parts
                        .by_ref()
                        .take_while(|&s| s != "moves")
                        .collect::<Vec<&str>>()
                        .join(" ");
                    self.board = Board::from_fen(&fen);
                }
                _ => {}
            }
        }

        for mv in parts {
            let parsed_move = Move::from_uci(mv);
            if let Some(mut mv) = parsed_move
                && let Some(piece) = self.board.get_piece_at(mv.from())
            {
                if piece % 6 == PAWN && (mv.to() as i32 - mv.from() as i32).abs() == 16 {
                    mv.append_flags(DOUBLE_PAWN_PUSH);
                }
                if piece % 6 == KING && (mv.to() as i32 - mv.from() as i32).abs() == 2 {
                    if mv.to() % 8 == 6 {
                        mv.append_flags(KING_CASTLE);
                    } else {
                        mv.append_flags(QUEEN_CASTLE);
                    }
                }
                if self.board.is_occupied(mv.to()) {
                    mv.append_flags(CAPTURE);
                }

                self.board.make_move(mv, piece % 6);
            }
        }
        self.debug(&format!("Castling rights: {}", self.board.castling_rights));
        self.debug(&format!("Position set to:\n{}", self.board));
    }

    fn handle_go(&mut self, parts: SplitWhitespace) {
        if let (eval, Some((best_move, _))) = self.board.minimax(5) {
            let cp = eval * if self.board.turn == 0 { 1 } else { -1 };
            println!(
                "info depth 1 score cp {} nodes 1234 nps 1234 time 1000 pv {}",
                cp, best_move
            );
            println!("bestmove {}", best_move);
            self.debug(&format!("Best move chosen: {}", best_move));
            //print_bitboard(self.board.attacked_squares(1 - self.board.turn));
        }
        //let iter = &mut parts.into_iter();
        //while let Some(param) = iter.next() {
        //    match param {
        //        _ => {}
        //    }
        //}
    }
}
