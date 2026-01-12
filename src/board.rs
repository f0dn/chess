use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::fs::OpenOptions;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use crate::consts::*;
use crate::eval::*;
use crate::moves::*;
use crate::zobrist::*;

pub fn print_bitboard(bb: u64) {
    for rank in (0..8).rev() {
        for file in 0..8 {
            let square = rank * 8 + file;
            let bit = (bb >> square) & 1;
            print!("{} ", if bit == 1 { '1' } else { '.' });
        }
        println!();
    }
    println!();
}

#[derive(Clone, PartialEq, Eq)]
pub struct Board {
    bitboards: [u64; 12],
    pub turn: u8,
    pub castling_rights: u8,
}

impl Hash for Board {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // TODO include en passant
        let mut hash: u64 = 0;
        for square in 0..64 {
            let mask = 1 << square;
            for (i, &bb) in self.bitboards.iter().enumerate() {
                if (bb & mask) != 0 {
                    hash ^= ZOBRIST_TABLE[square as usize][i];
                }
            }
        }
        if self.turn == 1 {
            hash ^= ZOBRIST_BLACK_TO_MOVE;
        }
        hash ^= ZOBRIST_CASTLING_RIGHTS[self.castling_rights as usize];
        state.write_u64(hash);
    }
}

impl Board {
    pub fn from_fen(fen: &str) -> Self {
        let mut board = Board {
            bitboards: [0; 12],
            turn: 0,
            castling_rights: 0,
        };

        let mut parts = fen.split_whitespace();
        let board_part = parts.next().unwrap();
        let mut rank = 7;
        let mut file = 0;
        for c in board_part.chars() {
            match c {
                '/' => {
                    rank -= 1;
                    file = 0;
                }
                '1'..='8' => {
                    file += c as u8 - b'0';
                }
                _ => {
                    let bit_index = rank * 8 + file;
                    let bb_index = Self::get_bb_index(c);
                    board.bitboards[bb_index] |= 1 << bit_index;
                    file += 1;
                }
            }
        }

        match parts.next() {
            Some("w") => board.turn = 0,
            Some("b") => board.turn = 1,
            _ => panic!("Invalid FEN: missing turn information"),
        }

        for c in parts.next().unwrap_or("").chars() {
            match c {
                'K' => board.castling_rights |= KING_CASTLE as u8,
                'Q' => board.castling_rights |= QUEEN_CASTLE as u8,
                'k' => board.castling_rights |= (KING_CASTLE as u8) << 2,
                'q' => board.castling_rights |= (QUEEN_CASTLE as u8) << 2,
                '-' => {}
                _ => panic!("Invalid FEN: invalid castling rights"),
            }
        }

        board
    }

    fn get_bb_index(piece: char) -> usize {
        match piece {
            'P' => PAWN,
            'N' => KNIGHT,
            'B' => BISHOP,
            'R' => ROOK,
            'Q' => QUEEN,
            'K' => KING,
            'p' => BLACK_PAWN,
            'n' => BLACK_KNIGHT,
            'b' => BLACK_BISHOP,
            'r' => BLACK_ROOK,
            'q' => BLACK_QUEEN,
            'k' => BLACK_KING,
            _ => panic!("Invalid piece character"),
        }
    }

    pub fn is_occupied(&self, square: u16) -> bool {
        let mask = 1 << square;
        for &bb in self.bitboards.iter() {
            if (bb & mask) != 0 {
                return true;
            }
        }
        false
    }

    pub fn get_piece_at(&self, square: u16) -> Option<usize> {
        let mask = 1 << square;
        for (i, &bb) in self.bitboards.iter().enumerate() {
            if (bb & mask) != 0 {
                return Some(i);
            }
        }
        None
    }

    fn pawns(&self) -> [u64; 2] {
        [self.bitboards[PAWN], self.bitboards[BLACK_PAWN]]
    }

    fn knights(&self) -> [u64; 2] {
        [self.bitboards[KNIGHT], self.bitboards[BLACK_KNIGHT]]
    }

    fn bishops(&self) -> [u64; 2] {
        [self.bitboards[BISHOP], self.bitboards[BLACK_BISHOP]]
    }

    fn rooks(&self) -> [u64; 2] {
        [self.bitboards[ROOK], self.bitboards[BLACK_ROOK]]
    }

    fn queens(&self) -> [u64; 2] {
        [self.bitboards[QUEEN], self.bitboards[BLACK_QUEEN]]
    }

    fn kings(&self) -> [u64; 2] {
        [self.bitboards[KING], self.bitboards[BLACK_KING]]
    }

    fn major_pieces(&self) -> u64 {
        self.add_bitboards(self.queens().as_ref()) | self.add_bitboards(self.rooks().as_ref())
    }

    fn minor_pieces(&self) -> u64 {
        self.add_bitboards(self.bishops().as_ref()) | self.add_bitboards(self.knights().as_ref())
    }

    fn is_checkmate_self(&self) -> bool {
        self.bitboards[KING + self.turn as usize * 6] == 0
    }

    fn is_checkmate_opp(&self) -> bool {
        self.bitboards[KING + (1 - self.turn) as usize * 6] == 0
    }

    fn is_draw(&self) -> bool {
        let num_major_pieces = self.major_pieces().count_ones();
        let num_pawns = self.add_bitboards(self.pawns().as_ref()).count_ones();
        let num_minor_pieces = self.minor_pieces().count_ones();
        num_major_pieces == 0 && num_pawns == 0 && num_minor_pieces <= 1
    }

    fn color_boards(&self, color: u8) -> [u64; 6] {
        self.bitboards[(color as usize * 6)..((color as usize + 1) * 6)]
            .try_into()
            .unwrap()
    }

    fn add_bitboards(&self, boards: &[u64]) -> u64 {
        boards.iter().fold(0, |acc, &bb| acc | bb)
    }

    fn opp_squares(&self) -> u64 {
        self.add_bitboards(&self.color_boards(1 - self.turn))
    }

    fn friendly_squares(&self) -> u64 {
        self.add_bitboards(&self.color_boards(self.turn))
    }

    fn piece_on_square(&self, square: u16) -> Option<usize> {
        let mask = 1 << square;
        for (i, &bb) in self.bitboards.iter().enumerate() {
            if (bb & mask) != 0 {
                return Some(i);
            }
        }
        None
    }

    fn evaluate(&self) -> Eval {
        let mut score = 0;

        for piece_type in 0..6 {
            for (mul, bb_index) in [(1, piece_type), (-1, piece_type + 6)] {
                let bitboard = self.bitboards[bb_index];
                let piece_value = PIECE_VALUES[piece_type];
                let square_values = &OPTIMAL_SQUARES[piece_type];

                score += mul * piece_value * bitboard.count_ones() as i32;
                score += mul
                    * square_values
                        .iter()
                        .enumerate()
                        .map(|(i, &val)| val * ((bitboard >> i) & 1) as i32)
                        .sum::<i32>();
            }
        }

        Eval::new(score)
    }

    fn sliding_moves(
        &self,
        dirs: &[usize],
        piece: usize,
        moves: &mut Vec<(usize, Move, usize)>,
        only_captures: bool,
    ) {
        let enemies = self.opp_squares();
        let friendly = self.friendly_squares();

        let mut piece_bb = self.bitboards[piece + (self.turn as usize * 6)];
        while piece_bb != 0 {
            let from_square = piece_bb.trailing_zeros() as u16;
            piece_bb &= piece_bb - 1;

            for &dir in dirs {
                let mut ray = SLIDING_MOVES[from_square as usize][dir];
                let mut unavaliable = 0;
                let blockers = ray & enemies;
                if blockers != 0 {
                    let closest = if dir < 4 {
                        blockers.trailing_zeros()
                    } else {
                        63 - blockers.leading_zeros()
                    };

                    if only_captures && (1 << closest) & friendly == 0 {
                        // do nothing
                    } else if only_captures {
                        moves.push((
                            piece,
                            Move::new(from_square, closest as u16, CAPTURE),
                            10 * PIECE_VALUES[self.piece_on_square(closest as u16).unwrap() % 6]
                                as usize
                                - PIECE_VALUES[piece] as usize,
                        ));
                        continue;
                    }

                    let blocker_ray = SLIDING_MOVES[closest as usize][dir];
                    unavaliable |= blocker_ray;
                }

                if only_captures {
                    continue;
                }

                let blockers = ray & friendly;
                if blockers != 0 {
                    let closest = if dir < 4 {
                        blockers.trailing_zeros()
                    } else {
                        63 - blockers.leading_zeros()
                    };

                    let blocker_ray = SLIDING_MOVES[closest as usize][dir] | (1 << closest);
                    unavaliable |= blocker_ray;
                }

                ray ^= unavaliable;

                while ray != 0 {
                    let to_square = ray.trailing_zeros() as u16;
                    ray &= ray - 1;

                    let (flags, value) = if (enemies & (1 << to_square)) != 0 {
                        (
                            CAPTURE,
                            10 * PIECE_VALUES[self.piece_on_square(to_square).unwrap() % 6]
                                as usize
                                - PIECE_VALUES[piece] as usize,
                        )
                    } else {
                        (0, 0)
                    };

                    moves.push((piece, Move::new(from_square, to_square, flags), value));
                }
            }
        }
    }

    pub fn attacked_squares(&self, color: u8) -> u64 {
        let mut attacks = 0;

        let enemies = self.add_bitboards(&self.color_boards(1 - color));
        let friendly = self.add_bitboards(&self.color_boards(color));

        for (piece, dirs) in [
            (BISHOP, vec![0, 2, 4, 6]),
            (ROOK, vec![1, 3, 5, 7]),
            (QUEEN, vec![0, 1, 2, 3, 4, 5, 6, 7]),
        ] {
            let mut piece_bb = self.bitboards[piece + (color as usize * 6)];
            while piece_bb != 0 {
                let from_square = piece_bb.trailing_zeros() as u16;
                piece_bb &= piece_bb - 1;

                for &dir in &dirs {
                    let mut ray = SLIDING_MOVES[from_square as usize][dir];
                    let mut unavaliable = 0;
                    let blockers = ray & enemies;
                    if blockers != 0 {
                        let closest = if dir < 4 {
                            blockers.trailing_zeros()
                        } else {
                            63 - blockers.leading_zeros()
                        };

                        let blocker_ray = SLIDING_MOVES[closest as usize][dir];
                        unavaliable |= blocker_ray;
                    }

                    let blockers = ray & friendly;
                    if blockers != 0 {
                        let closest = if dir < 4 {
                            blockers.trailing_zeros()
                        } else {
                            63 - blockers.leading_zeros()
                        };

                        let blocker_ray = SLIDING_MOVES[closest as usize][dir] | (1 << closest);
                        unavaliable |= blocker_ray;
                    }

                    ray ^= unavaliable;

                    attacks |= ray;
                }
            }
        }

        let dir = if color == 0 { 8 } else { -8 };

        let mut piece_bb = self.bitboards[PAWN + (color as usize * 6)];
        while piece_bb != 0 {
            let from_square = piece_bb.trailing_zeros() as u16;
            piece_bb &= piece_bb - 1;

            for &attack_dir in &[-1, 1] {
                if (from_square.is_multiple_of(8) && attack_dir == -1)
                    || (from_square % 8 == 7 && attack_dir == 1)
                {
                    continue;
                }
                let to_square = (from_square as isize + dir + attack_dir) as u16;
                if to_square < 64 && (enemies & (1 << to_square)) != 0 {
                    attacks |= 1 << to_square;
                }
            }
        }

        for (piece, moves_array) in [(KNIGHT, &KNIGHT_MOVES), (KING, &KING_MOVES)] {
            let mut bb = self.bitboards[piece + (color as usize * 6)];
            while bb != 0 {
                let from_square = bb.trailing_zeros() as u16;
                bb &= bb - 1;

                let possible_moves = moves_array[from_square as usize];

                let mut non_friendly = possible_moves & !friendly;

                while non_friendly != 0 {
                    let to_square = non_friendly.trailing_zeros() as u16;
                    non_friendly &= non_friendly - 1;

                    attacks |= 1 << to_square;
                }
            }
        }

        attacks
    }

    fn add_pawn_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        let dir = if self.turn == 0 { 8 } else { -8 };

        let enemies = self.opp_squares();
        let friendly = self.friendly_squares();

        let mut piece_bb = self.bitboards[PAWN + (self.turn as usize * 6)];
        while piece_bb != 0 {
            let from_square = piece_bb.trailing_zeros() as u16;
            piece_bb &= piece_bb - 1;

            let to_square = (from_square as isize + dir) as u16;
            if !only_captures && to_square < 64 && (1 << to_square) & (enemies | friendly) == 0 {
                let rank = to_square / 8;
                if rank == 0 || rank == 7 {
                    let promotion_pieces = [QUEEN, ROOK, BISHOP, KNIGHT];
                    for &promo_piece in &promotion_pieces {
                        moves.push((
                            PAWN,
                            Move::new(from_square, to_square, PROMOTION | promo_piece as u16),
                            PIECE_VALUES[promo_piece] as usize,
                        ));
                    }
                } else {
                    moves.push((PAWN, Move::new(from_square, to_square, 0), 0));
                };

                if (from_square / 8 == 1 && self.turn == 0)
                    || (from_square / 8 == 6 && self.turn == 1)
                {
                    let to_square_2 = (from_square as isize + 2 * dir) as u16;
                    if to_square < 64 && (1 << to_square_2) & (enemies | friendly) == 0 {
                        moves.push((
                            PAWN,
                            Move::new(from_square, to_square_2, DOUBLE_PAWN_PUSH),
                            0,
                        ));
                    }
                }
            }

            for &attack_dir in &[-1, 1] {
                if (from_square.is_multiple_of(8) && attack_dir == -1)
                    || (from_square % 8 == 7 && attack_dir == 1)
                {
                    continue;
                }
                let to_square = (from_square as isize + dir + attack_dir) as u16;
                if to_square < 64 && (enemies & (1 << to_square)) != 0 {
                    let rank = to_square / 8;
                    let captured_piece = self.piece_on_square(to_square).unwrap() % 6;
                    if rank == 0 || rank == 7 {
                        let promotion_pieces = [QUEEN, ROOK, BISHOP, KNIGHT];
                        for &promo_piece in &promotion_pieces {
                            moves.push((
                                PAWN,
                                Move::new(
                                    from_square,
                                    to_square,
                                    PROMOTION | CAPTURE | promo_piece as u16,
                                ),
                                PIECE_VALUES[promo_piece] as usize
                                    + 10 * PIECE_VALUES[captured_piece] as usize
                                    - PIECE_VALUES[PAWN] as usize,
                            ));
                        }
                    } else {
                        moves.push((
                            PAWN,
                            Move::new(from_square, to_square, CAPTURE),
                            10 * PIECE_VALUES[captured_piece] as usize
                                - PIECE_VALUES[PAWN] as usize,
                        ));
                    };
                }
            }
        }
    }

    fn add_knight_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        let enemies = self.opp_squares();
        let friendly = self.friendly_squares();

        let mut knight_bb = self.bitboards[KNIGHT + (self.turn as usize * 6)];
        while knight_bb != 0 {
            let from_square = knight_bb.trailing_zeros() as u16;
            knight_bb &= knight_bb - 1;

            let possible_moves = KNIGHT_MOVES[from_square as usize];

            let mut non_friendly = possible_moves & !friendly;

            while non_friendly != 0 {
                let to_square = non_friendly.trailing_zeros() as u16;
                non_friendly &= non_friendly - 1;

                let (flags, value) = if enemies & (1 << to_square) != 0 {
                    (
                        CAPTURE,
                        10 * PIECE_VALUES[self.piece_on_square(to_square).unwrap() % 6] as usize
                            - PIECE_VALUES[KNIGHT] as usize,
                    )
                } else {
                    (0, 0)
                };

                if only_captures && flags == 0 {
                    continue;
                }

                moves.push((KNIGHT, Move::new(from_square, to_square, flags), value));
            }
        }
    }

    fn add_king_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        let enemies = self.opp_squares();
        let friendly = self.friendly_squares();

        let mut king_bb = self.bitboards[KING + (self.turn as usize * 6)];
        while king_bb != 0 {
            let from_square = king_bb.trailing_zeros() as u16;
            king_bb &= king_bb - 1;

            let possible_moves = KING_MOVES[from_square as usize];

            let mut non_friendly = possible_moves & !friendly;

            while non_friendly != 0 {
                let to_square = non_friendly.trailing_zeros() as u16;
                non_friendly &= non_friendly - 1;

                let (flags, value) = if enemies & (1 << to_square) != 0 {
                    (CAPTURE, 0)
                } else {
                    (0, 0)
                };

                if only_captures && flags == 0 {
                    continue;
                }

                moves.push((KING, Move::new(from_square, to_square, flags), value));
            }
        }
    }

    fn add_bishop_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        self.sliding_moves(&[0, 2, 4, 6], BISHOP, moves, only_captures)
    }

    fn add_rook_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        self.sliding_moves(&[1, 3, 5, 7], ROOK, moves, only_captures)
    }

    fn add_castling_moves(&self, moves: &mut Vec<(usize, Move, usize)>) {
        let rights = self.castling_rights >> (self.turn * 2);

        let mut possible = false;

        let occupied = self.add_bitboards(&self.bitboards);

        for dir in [QUEEN_CASTLE, KING_CASTLE] {
            if rights & dir as u8 == 0 {
                continue;
            }

            if CASTLING_EMPTY[self.turn as usize][dir as usize - 1] & occupied != 0 {
                continue;
            }

            possible = true;
        }

        if !possible {
            return;
        }

        let attacked = self.attacked_squares(1 - self.turn);

        for dir in [QUEEN_CASTLE, KING_CASTLE] {
            if rights & dir as u8 == 0 {
                continue;
            }

            if CASTLING_EMPTY[self.turn as usize][dir as usize - 1] & occupied != 0 {
                continue;
            }

            if CASTLING_NON_ATTACKED[self.turn as usize][dir as usize - 1] & attacked != 0 {
                continue;
            }

            let from_square = CASTLING_KING_FROM_SQUARE[self.turn as usize];
            let to_square = CASTLING_KING_TO_SQUARE[self.turn as usize][dir as usize - 1];
            let flags = dir;

            moves.push((KING, Move::new(from_square, to_square, flags), 100));
        }
    }

    fn add_queen_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        self.sliding_moves(&[0, 1, 2, 3, 4, 5, 6, 7], QUEEN, moves, only_captures)
    }

    fn add_moves(&self, moves: &mut Vec<(usize, Move, usize)>, only_captures: bool) {
        self.add_pawn_moves(moves, only_captures);
        self.add_knight_moves(moves, only_captures);
        self.add_bishop_moves(moves, only_captures);
        self.add_rook_moves(moves, only_captures);
        self.add_queen_moves(moves, only_captures);
        self.add_king_moves(moves, only_captures);
        if !only_captures {
            self.add_castling_moves(moves);
        }
    }

    pub fn make_move(&mut self, m: &Move, piece: usize) {
        let flags = m.flags();
        let from_mask = 1 << m.from();
        let to_mask = 1 << m.to();
        let opp_to_mask = !to_mask;

        self.bitboards[piece + 6 * self.turn as usize] ^= from_mask | to_mask;

        if piece == KING {
            self.castling_rights &=
                (KING_CASTLE as u8 | QUEEN_CASTLE as u8) << ((1 - self.turn) * 2);
        }

        if piece == ROOK {
            for dir in [KING_CASTLE, QUEEN_CASTLE] {
                if m.from() == CASTLING_ROOK_FROM_SQUARE[self.turn as usize][dir as usize - 1] {
                    self.castling_rights &= !((dir as u8) << (self.turn * 2));
                }
            }
        }

        if flags & CAPTURE != 0 {
            for i in (6 * (1 - self.turn))..(6 * ((1 - self.turn) + 1)) {
                self.bitboards[i as usize] &= opp_to_mask;
            }

            for dir in [KING_CASTLE, QUEEN_CASTLE] {
                if m.to() == CASTLING_ROOK_FROM_SQUARE[1 - self.turn as usize][dir as usize - 1] {
                    self.castling_rights &= !((dir as u8) << ((1 - self.turn) * 2));
                }
            }
        }

        if flags & PROMOTION != 0 {
            self.bitboards[PAWN + self.turn as usize * 6] ^= to_mask;
            self.bitboards[(flags & 0b11) as usize + self.turn as usize * 6] ^= to_mask;
        }

        if flags == QUEEN_CASTLE || flags == KING_CASTLE {
            self.bitboards[ROOK + self.turn as usize * 6] ^=
                CASTLING_ROOK_MASK[self.turn as usize][flags as usize - 1];
        }

        self.turn = 1 - self.turn;
    }

    fn capture_search(
        &self,
        mut alpha: Eval,
        mut beta: Eval,
        cancel: Arc<AtomicBool>,
        memo: &mut HashMap<Board, (Eval, Vec<Move>)>,
    ) -> (Eval, usize, Vec<Move>) {
        {
            if cancel.load(Ordering::Relaxed) {
                return (DRAW_EVAL, 0, Vec::new());
            }
        }

        let mut moves = Vec::new();
        if let Some(&(stored_score, ref stored_line)) = memo.get(self) {
            return (stored_score, 1, stored_line.clone());
        }

        if self.is_checkmate_self() {
            return if self.turn == 0 {
                (
                    Eval::mate_in(1), // TODO THIS IS BAD
                    1,
                    Vec::new(),
                )
            } else {
                (-Eval::mate_in(1), 1, Vec::new())
            };
        }

        if self.is_checkmate_opp() {
            return if self.turn == 0 {
                (-Eval::mate_in(1), 1, Vec::new())
            } else {
                (Eval::mate_in(1), 1, Vec::new())
            };
        }

        if self.is_draw() {
            return (DRAW_EVAL, 1, Vec::new());
        }

        let mut best_score = self.evaluate();

        let mut best_line = Vec::new();
        let mut nodes = 0;

        self.add_moves(&mut moves, true);

        //self.debug(&format!("num moves: {}", moves.len()));

        moves.sort_by(|a, b| b.2.cmp(&a.2));

        for (piece, m, _) in &moves {
            let mut new_board = self.clone();
            //self.debug(&format!("Making move: {}", m));
            new_board.make_move(m, *piece);
            let mut score = new_board.capture_search(alpha, beta, cancel.clone(), memo);
            score.2.push(*m);
            nodes += score.1;

            if self.turn == 0 {
                if score.0 > best_score {
                    best_score = score.0;
                    best_line = score.2;
                }
                if best_score >= beta {
                    break;
                }
                alpha = alpha.max(best_score);
            } else if score.0 < best_score {
                best_score = score.0;
                best_line = score.2;
                if best_score <= alpha {
                    break;
                }
                beta = beta.min(best_score);
            }
        }

        memo.insert(self.clone(), (best_score, best_line.clone()));
        //self.debug(&format!("returning score: {}", best_score));
        (best_score, nodes, best_line)
    }

    fn search(
        &self,
        mut alpha: Eval,
        mut beta: Eval,
        depth: usize,
        start_depth: usize,
        cancel: Arc<AtomicBool>,
        memo: &mut HashMap<Board, (usize, Eval, Vec<Move>, Vec<(usize, Move, usize)>)>,
    ) -> (Eval, usize, Vec<Move>) {
        {
            if cancel.load(Ordering::Relaxed) {
                return (DRAW_EVAL, 0, Vec::new());
            }
        }

        let mut moves = Vec::new();
        if let Some(&(stored_depth, stored_score, ref stored_line, ref stored_moves)) =
            memo.get(self)
        {
            if stored_depth >= depth {
                return (stored_score, 1, stored_line.clone());
            } else {
                moves = stored_moves.clone();
            }
        }

        if self.is_checkmate_self() {
            return if self.turn == 0 {
                (
                    Eval::mate_in(start_depth as i32 - depth as i32 - 1),
                    1,
                    Vec::new(),
                )
            } else {
                (
                    -Eval::mate_in(start_depth as i32 - depth as i32 - 1),
                    1,
                    Vec::new(),
                )
            };
        }

        if self.is_checkmate_opp() {
            return if self.turn == 0 {
                (
                    -Eval::mate_in(start_depth as i32 - depth as i32 - 1),
                    1,
                    Vec::new(),
                )
            } else {
                (
                    Eval::mate_in(start_depth as i32 - depth as i32 - 1),
                    1,
                    Vec::new(),
                )
            };
        }

        if self.is_draw() {
            return (DRAW_EVAL, 1, Vec::new());
        }

        if depth == 0 {
            let mut memo2 = HashMap::new();
            return self.capture_search(alpha, beta, cancel, &mut memo2);
        }

        let mut best_score = if self.turn == 0 { MIN_EVAL } else { MAX_EVAL };

        let mut best_line = Vec::new();
        let mut nodes = 0;

        self.add_moves(&mut moves, false);

        moves.sort_by(|a, b| b.2.cmp(&a.2));

        //self.debug(&format!(
        //    "Depth: {}, Moves generated: {}",
        //    depth,
        //    moves.len()
        //));

        for (piece, m, _) in &moves {
            let mut new_board = self.clone();
            //self.debug(&format!("Making move: {}", m));
            new_board.make_move(m, *piece);
            let mut score =
                new_board.search(alpha, beta, depth - 1, start_depth, cancel.clone(), memo);
            score.2.push(*m);
            nodes += score.1;

            if self.turn == 0 {
                if score.0 > best_score {
                    best_score = score.0;
                    best_line = score.2;
                }
                if best_score >= beta {
                    break;
                }
                alpha = alpha.max(best_score);
            } else if score.0 < best_score {
                best_score = score.0;
                best_line = score.2;
                if best_score <= alpha {
                    break;
                }
                beta = beta.min(best_score);
            }
        }

        memo.insert(self.clone(), (depth, best_score, best_line.clone(), moves));
        //self.debug(&format!("returning score: {}", best_score));
        (best_score, nodes, best_line)
    }

    pub fn minimax(
        &self,
        depth: usize,
        cancel: Arc<AtomicBool>,
        memo: &mut HashMap<Board, (usize, Eval, Vec<Move>, Vec<(usize, Move, usize)>)>,
    ) -> (Eval, usize, Vec<Move>) {
        let res = self.search(MIN_EVAL, MAX_EVAL, depth, depth, cancel, memo);
        self.debug(&format!("Minimax result: {}", res.0));
        res
    }

    fn debug(&self, msg: &str) {
        let debug_file = "debug.txt";

        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(debug_file)
            .unwrap();

        writeln!(file, "{}", msg).unwrap();
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for rank in (0..8).rev() {
            write!(f, "{} ", rank + 1)?;
            for file in 0..8 {
                let square = rank * 8 + file;
                let mut piece_char = '.';

                for (i, &bb) in self.bitboards.iter().enumerate() {
                    if (bb & (1 << square)) != 0 {
                        piece_char = match i {
                            PAWN => 'P',
                            KNIGHT => 'N',
                            BISHOP => 'B',
                            ROOK => 'R',
                            QUEEN => 'Q',
                            KING => 'K',
                            BLACK_PAWN => 'p',
                            BLACK_KNIGHT => 'n',
                            BLACK_BISHOP => 'b',
                            BLACK_ROOK => 'r',
                            BLACK_QUEEN => 'q',
                            BLACK_KING => 'k',
                            _ => unreachable!(),
                        };
                        break;
                    }
                }

                write!(f, "{} ", piece_char)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "  a b c d e f g h")
    }
}
