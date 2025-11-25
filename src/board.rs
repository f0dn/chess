use std::fmt::Display;

use crate::consts::*;
use crate::moves::*;

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

    pub fn sliding_moves(&self, dirs: &[usize], piece: usize) -> Vec<Move> {
        let mut moves = Vec::new();

        let enemies = self.opp_squares();
        let friendly = self.friendly_squares();

        let mut piece_bb = self.bitboards[piece + (self.turn as usize * 6)];
        while piece_bb != 0 {
            let from_square = piece_bb.trailing_zeros() as u16;
            piece_bb &= piece_bb - 1;

            for &dir in dirs {
                let mut ray = SLIDING_MOVES[from_square as usize][dir];
                let blockers = ray & enemies;
                if blockers != 0 {
                    let closest = if dir < 4 {
                        blockers.trailing_zeros()
                    } else {
                        blockers.leading_zeros()
                    };

                    let blocker_ray = SLIDING_MOVES[closest as usize][dir];
                    ray ^= blocker_ray;
                }

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
