use std::fmt::Display;

use crate::consts::*;
use crate::moves::*;

fn print_bitboard(bb: u64) {
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

#[derive(Clone)]
pub struct Board {
    bitboards: [u64; 12],
    turn: u8,
}

impl Board {
    pub fn from_fen(fen: &str) -> Self {
        let mut board = Board {
            bitboards: [0; 12],
            turn: 0,
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

    fn is_checkmate(&self) -> bool {
        self.kings().iter().sum::<u64>() == 0
    }

    fn color_boards(&self, color: u8) -> [u64; 6] {
        self.bitboards[(color as usize * 6)..((color as usize + 1) * 6)]
            .try_into()
            .unwrap()
    }

    fn white_boards(&self) -> [u64; 6] {
        self.color_boards(0)
    }

    fn black_boards(&self) -> [u64; 6] {
        self.color_boards(1)
    }

    fn add_bitboards(&self, boards: &[u64; 6]) -> u64 {
        boards.iter().fold(0, |acc, &bb| acc | bb)
    }

    fn opp_squares(&self) -> u64 {
        self.add_bitboards(&self.color_boards(1 - self.turn))
    }

    fn friendly_squares(&self) -> u64 {
        self.add_bitboards(&self.color_boards(self.turn))
    }

    fn evaluate(&self) -> i32 {
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

        score
    }

    fn sliding_moves(&self, dirs: &[usize], piece: usize) -> Vec<Move> {
        let mut moves = Vec::new();

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

                while ray != 0 {
                    let to_square = ray.trailing_zeros() as u16;
                    ray &= ray - 1;

                    let flags = if (enemies & (1 << to_square)) != 0 {
                        CAPTURE
                    } else {
                        0
                    };

                    moves.push(Move::new(from_square, to_square, flags));
                }
            }
        }

        moves
    }

    pub fn pawn_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();

        let dir = if self.turn == 0 { 8 } else { -8 };

        let enemies = self.opp_squares();
        let friendly = self.friendly_squares();

        let mut piece_bb = self.bitboards[PAWN + (self.turn as usize * 6)];
        while piece_bb != 0 {
            let from_square = piece_bb.trailing_zeros() as u16;
            piece_bb &= piece_bb - 1;

            let to_square = (from_square as isize + dir) as u16;
            if to_square < 64 && (1 << to_square) & (enemies | friendly) == 0 {
                let rank = to_square / 8;
                if rank == 0 || rank == 7 {
                    let promotion_pieces = [QUEEN, ROOK, BISHOP, KNIGHT];
                    for &promo_piece in &promotion_pieces {
                        moves.push(Move::new(
                            from_square,
                            to_square,
                            PROMOTION | promo_piece as u16,
                        ));
                    }
                } else {
                    moves.push(Move::new(from_square, to_square, 0));
                };

                if (from_square / 8 == 1 && self.turn == 0)
                    || (from_square / 8 == 6 && self.turn == 1)
                {
                    let to_square_2 = (from_square as isize + 2 * dir) as u16;
                    if to_square < 64 && (1 << to_square_2) & (enemies | friendly) == 0 {
                        moves.push(Move::new(from_square, to_square_2, DOUBLE_PAWN_PUSH));
                    }
                }
            }

            for &attack_dir in &[-1, 1] {
                if (from_square % 8 == 0 && attack_dir == -1)
                    || (from_square % 8 == 7 && attack_dir == 1)
                {
                    continue;
                }
                let to_square = (from_square as isize + dir + attack_dir) as u16;
                if to_square < 64 && (enemies & (1 << to_square)) != 0 {
                    let rank = to_square / 8;
                    if rank == 0 || rank == 7 {
                        let promotion_pieces = [QUEEN, ROOK, BISHOP, KNIGHT];
                        for &promo_piece in &promotion_pieces {
                            moves.push(Move::new(
                                from_square,
                                to_square,
                                PROMOTION | CAPTURE | promo_piece as u16,
                            ));
                        }
                    } else {
                        moves.push(Move::new(from_square, to_square, CAPTURE));
                    };
                }
            }
        }

        moves
    }

    fn knight_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();

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

                let flags = if enemies & (1 << to_square) != 0 {
                    CAPTURE
                } else {
                    0
                };

                moves.push(Move::new(from_square, to_square, flags));
            }
        }

        moves
    }

    fn king_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();

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

                let flags = if enemies & (1 << to_square) != 0 {
                    CAPTURE
                } else {
                    0
                };

                moves.push(Move::new(from_square, to_square, flags));
            }
        }

        moves
    }

    fn bishop_moves(&self) -> Vec<Move> {
        self.sliding_moves(&[0, 2, 4, 6], BISHOP)
    }

    fn rook_moves(&self) -> Vec<Move> {
        self.sliding_moves(&[1, 3, 5, 7], ROOK)
    }

    fn queen_moves(&self) -> Vec<Move> {
        self.sliding_moves(&[0, 1, 2, 3, 4, 5, 6, 7], QUEEN)
    }

    pub fn make_move(&mut self, m: Move, piece: usize) {
        let flags = m.flags();
        let from_mask = 1 << m.from();
        let to_mask = 1 << m.to();
        let opp_to_mask = !to_mask;

        self.bitboards[piece + 6 * self.turn as usize] ^= from_mask | to_mask;

        if flags & CAPTURE != 0 {
            for i in (6 * (1 - self.turn))..(6 * ((1 - self.turn) + 1)) {
                self.bitboards[i as usize] &= opp_to_mask;
            }
        }

        if flags & PROMOTION != 0 {
            self.bitboards[PAWN + self.turn as usize * 6] ^= to_mask;
            self.bitboards[(flags & 0b11) as usize + self.turn as usize * 6] ^= to_mask;
        }

        self.turn = 1 - self.turn;
    }

    pub fn alphabeta(
        &self,
        mut alpha: i32,
        mut beta: i32,
        depth: usize,
    ) -> (i32, Option<(Move, usize)>) {
        if depth == 0 {
            return (self.evaluate(), None);
        }

        let mut best_score = if self.turn == 0 { i32::MIN } else { i32::MAX };

        let mut best_move = None;

        let moves = vec![
            (PAWN, self.pawn_moves()),
            (KNIGHT, self.knight_moves()),
            (BISHOP, self.bishop_moves()),
            (ROOK, self.rook_moves()),
            (QUEEN, self.queen_moves()),
            (KING, self.king_moves()),
        ];

        for (piece, piece_moves) in moves {
            for m in piece_moves {
                let mut new_board = self.clone();
                new_board.make_move(m, piece);
                let score = new_board.alphabeta(alpha, beta, depth - 1);

                if self.turn == 0 {
                    if score.0 > best_score {
                        best_score = score.0;
                        best_move = Some((m, piece));
                    }
                    if best_score >= beta {
                        break;
                    }
                    alpha = alpha.max(best_score);
                } else if score.0 < best_score {
                    best_score = score.0;
                    best_move = Some((m, piece));
                    if best_score <= alpha {
                        break;
                    }
                    beta = beta.min(best_score);
                }
            }
        }

        (best_score, best_move)
    }

    pub fn minimax(&self, depth: usize) -> (i32, Option<(Move, usize)>) {
        self.alphabeta(i32::MIN, i32::MAX, depth)
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
