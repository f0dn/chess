use std::collections::HashMap;
use std::str::SplitWhitespace;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::thread::spawn;
use std::time::Duration;
use std::time::Instant;

use crate::board::Board;
use crate::board::print_bitboard;
use crate::consts::*;
use crate::moves::*;
use std::fs::OpenOptions;
use std::io::Write;

struct GoOptions {
    ponder: bool,
    wtime: Option<usize>,
    btime: Option<usize>,
    winc: Option<usize>,
    binc: Option<usize>,
    movestogo: Option<usize>,
    depth: Option<usize>,
    nodes: Option<usize>,
    mate: Option<usize>,
    movetime: Option<usize>,
    infinite: bool,
}

impl GoOptions {
    fn parse(mut parts: SplitWhitespace) -> Self {
        let mut options = GoOptions {
            ponder: false,
            wtime: None,
            btime: None,
            winc: None,
            binc: None,
            movestogo: None,
            depth: None,
            nodes: None,
            mate: None,
            movetime: None,
            infinite: false,
        };
        while let Some(s) = parts.next() {
            match s {
                "ponder" => options.ponder = true,
                "wtime" => {
                    if let Some(value) = parts.next() {
                        options.wtime = value.parse().ok();
                    }
                }
                "btime" => {
                    if let Some(value) = parts.next() {
                        options.btime = value.parse().ok();
                    }
                }
                "winc" => {
                    if let Some(value) = parts.next() {
                        options.winc = value.parse().ok();
                    }
                }
                "binc" => {
                    if let Some(value) = parts.next() {
                        options.binc = value.parse().ok();
                    }
                }
                "movestogo" => {
                    if let Some(value) = parts.next() {
                        options.movestogo = value.parse().ok();
                    }
                }
                "depth" => {
                    if let Some(value) = parts.next() {
                        options.depth = value.parse().ok();
                    }
                }
                "nodes" => {
                    if let Some(value) = parts.next() {
                        options.nodes = value.parse().ok();
                    }
                }
                "mate" => {
                    if let Some(value) = parts.next() {
                        options.mate = value.parse().ok();
                    }
                }
                "movetime" => {
                    if let Some(value) = parts.next() {
                        options.movetime = value.parse().ok();
                    }
                }
                "infinite" => options.infinite = true,
                _ => {}
            }
        }
        options
    }
}

pub struct Engine {
    board: Arc<Mutex<Board>>,
    cancel: Arc<AtomicBool>,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            board: Arc::new(Mutex::new(Board::from_fen(
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            ))),
            cancel: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn start(&mut self) {
        while let Some(command) = self.read_command() {
            Engine::debug(&format!("Received command: {}", command));
            let mut parts = command.split_whitespace();
            match parts.next().unwrap_or("") {
                "uci" => self.handle_uci(),
                "isready" => self.handle_isready(),
                "position" => self.handle_position(parts),
                "go" => self.handle_go(parts),
                "stop" => {
                    self.cancel.store(true, Ordering::Relaxed);
                }
                "quit" => break,
                _ => (),
            }
        }
    }

    fn debug(msg: &str) {
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
                    self.board = Arc::new(Mutex::new(Board::from_fen(
                        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                    )));
                }
                "fen" => {
                    let fen: String = parts
                        .by_ref()
                        .take_while(|&s| s != "moves")
                        .collect::<Vec<&str>>()
                        .join(" ");
                    self.board = Arc::new(Mutex::new(Board::from_fen(&fen)));
                }
                _ => {}
            }
        }

        let mut board = self.board.lock().unwrap();
        for mv in parts {
            let parsed_move = Move::from_uci(mv);
            if let Some(mut mv) = parsed_move
                && let Some(piece) = board.get_piece_at(mv.from())
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
                if board.is_occupied(mv.to()) {
                    mv.append_flags(CAPTURE);
                }

                board.make_move(&mv, piece % 6);
            }
        }
        Engine::debug(&format!("Castling rights: {}", board.castling_rights));
        Engine::debug(&format!("Position set to:\n{}", board));
    }

    fn handle_go(&mut self, parts: SplitWhitespace) {
        self.cancel.store(false, Ordering::Relaxed);
        let thread_board = self.board.clone();
        let thread_cancel = self.cancel.clone();
        let options = GoOptions::parse(parts);
        spawn(move || {
            let mut evals = HashMap::new();
            let board = thread_board.lock().unwrap();
            let time_start = Instant::now();
            let time_limit = if options.infinite {
                usize::MAX
            } else if board.turn == 0 {
                options.btime.unwrap_or(1000) + options.binc.unwrap_or(0)
            } else {
                options.wtime.unwrap_or(1000) + options.winc.unwrap_or(0)
            };
            let time_allowed = (time_limit / 50).max(2000);
            let end_time = time_start + Duration::from_millis(time_allowed as u64);
            let time_thread_cancel = thread_cancel.clone();
            spawn(move || {
                while Instant::now() < end_time && !time_thread_cancel.load(Ordering::Relaxed) {
                    std::thread::sleep(Duration::from_millis(10));
                }
                time_thread_cancel.store(true, Ordering::Relaxed);
            });
            let mut depth = 1;
            let mut best_move = Move::new(0, 0, 0);
            loop {
                let start = Instant::now();
                let (eval, nodes, best_line) =
                    board.minimax(depth, thread_cancel.clone(), &mut evals);
                let duration = Instant::now() - start;
                if thread_cancel.load(Ordering::Relaxed) {
                    break;
                }
                best_move = best_line[best_line.len() - 1];
                let cp = if board.turn == 0 { eval } else { -eval };
                println!(
                    "info depth {} nodes {} nps {} time {} score {} pv {}",
                    depth,
                    nodes,
                    nodes as u128 * 1000 / duration.as_millis().max(1),
                    duration.as_millis(),
                    cp,
                    best_line
                        .iter()
                        .rev()
                        .map(|m| m.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                );
                //print_bitboard(self.board.attacked_squares(1 - self.board.turn));
                depth += 1;
            }
            Engine::debug(&format!("Best move chosen: {}", best_move));
            println!("bestmove {}", best_move);
        });
        //let iter = &mut parts.into_iter();
        //while let Some(param) = iter.next() {
        //    match param {
        //        _ => {}
        //    }
        //}
    }
}
