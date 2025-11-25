use crate::consts::*;
use crate::moves::*;

struct Board {
    bitboards: [u64; 12],
    turn: u8,
}

impl Board {
    fn from_fen(fen: &str) -> Self {
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
            'p' => PAWN + 6,
            'n' => KNIGHT + 6,
            'b' => BISHOP + 6,
            'r' => ROOK + 6,
            'q' => QUEEN + 6,
            'k' => KING + 6,
            _ => panic!("Invalid piece character"),
        }
    }

    fn pawns(&self) -> [u64; 2] {
        [self.bitboards[PAWN], self.bitboards[PAWN + 6]]
    }

    fn knights(&self) -> [u64; 2] {
        [self.bitboards[KNIGHT], self.bitboards[KNIGHT + 6]]
    }

    fn bishops(&self) -> [u64; 2] {
        [self.bitboards[BISHOP], self.bitboards[BISHOP + 6]]
    }

    fn rooks(&self) -> [u64; 2] {
        [self.bitboards[ROOK], self.bitboards[ROOK + 6]]
    }

    fn queens(&self) -> [u64; 2] {
        [self.bitboards[QUEEN], self.bitboards[QUEEN + 6]]
    }

    fn kings(&self) -> [u64; 2] {
        [self.bitboards[KING], self.bitboards[KING + 6]]
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

    fn opp_squares(&self) -> u64 {
        self.color_boards(1 - self.turn)
            .iter()
            .fold(0, |acc, &bb| acc | bb)
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

                    let blocker_ray = SLIDING_MOVES[from_square as usize][dir];
                    ray ^= blocker_ray;
                }
            }
        }

        moves
    }
}
