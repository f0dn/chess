pub const KNIGHT: usize = 0;
pub const BISHOP: usize = 1;
pub const ROOK: usize = 2;
pub const QUEEN: usize = 3;
pub const PAWN: usize = 4;
pub const KING: usize = 5;

pub const BLACK_KNIGHT: usize = KNIGHT + 6;
pub const BLACK_BISHOP: usize = BISHOP + 6;
pub const BLACK_ROOK: usize = ROOK + 6;
pub const BLACK_QUEEN: usize = QUEEN + 6;
pub const BLACK_PAWN: usize = PAWN + 6;
pub const BLACK_KING: usize = KING + 6;

pub const PAWN_SQUARES: [i32; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0, //
    50, 50, 50, 50, 50, 50, 50, 50, //
    10, 10, 20, 30, 30, 20, 10, 10, //
    5, 5, 10, 25, 25, 10, 5, 5, //
    0, 0, 0, 20, 20, 0, 0, 0, //
    5, -5, -10, 0, 0, -10, -5, 5, //
    5, 10, 10, -20, -20, 10, 10, 5, //
    0, 0, 0, 0, 0, 0, 0, 0,
];

pub const KNIGHT_SQUARES: [i32; 64] = [
    -50, -40, -30, -30, -30, -30, -40, -50, //
    -40, -20, 0, 0, 0, 0, -20, -40, //
    -30, 0, 10, 15, 15, 10, 0, -30, //
    -30, 5, 15, 20, 20, 15, 5, -30, //
    -30, 0, 15, 20, 20, 15, 0, -30, //
    -30, 5, 10, 15, 15, 10, 5, -30, //
    -40, -20, 0, 5, 5, 0, -20, -40, //
    -50, -40, -30, -30, -30, -30, -40, -50,
];

pub const BISHOP_SQUARES: [i32; 64] = [
    -20, -10, -10, -10, -10, -10, -10, -20, //
    -10, 0, 0, 0, 0, 0, 0, -10, //
    -10, 0, 5, 10, 10, 5, 0, -10, //
    -10, 5, 5, 10, 10, 5, 5, -10, //
    -10, 0, 10, 10, 10, 10, 0, -10, //
    -10, 10, 10, 10, 10, 10, 10, -10, //
    -10, 5, 0, 0, 0, 0, 5, -10, //
    -20, -10, -10, -10, -10, -10, -10, -20,
];

pub const ROOK_SQUARES: [i32; 64] = [
    0, 0, 0, 0, 0, 0, 0, 0, //
    5, 10, 10, 10, 10, 10, 10, 5, //
    -5, 0, 0, 0, 0, 0, 0, -5, //
    -5, 0, 0, 0, 0, 0, 0, -5, //
    -5, 0, 0, 0, 0, 0, 0, -5, //
    -5, 0, 0, 0, 0, 0, 0, -5, //
    -5, 0, 0, 0, 0, 0, 0, -5, //
    0, 0, 0, 5, 5, 0, 0, 0,
];

pub const QUEEN_SQUARES: [i32; 64] = [
    -20, -10, -10, -5, -5, -10, -10, -20, //
    -10, 0, 0, 0, 0, 0, 0, -10, //
    -10, 0, 5, 5, 5, 5, 0, -10, //
    -5, 0, 5, 5, 5, 5, 0, -5, //
    0, 0, 5, 5, 5, 5, 0, -5, //
    -10, 5, 5, 5, 5, 5, 0, -10, //
    -10, 0, 5, 0, 0, 0, 0, -10, //
    -20, -10, -10, -5, -5, -10, -10, -20,
];

pub const KING_SQUARES: [i32; 64] = [
    -30, -40, -40, -50, -50, -40, -40, -30, //
    -30, -40, -40, -50, -50, -40, -40, -30, //
    -30, -40, -40, -50, -50, -40, -40, -30, //
    -30, -40, -40, -50, -50, -40, -40, -30, //
    -20, -30, -30, -40, -40, -30, -30, -20, //
    -10, -20, -20, -20, -20, -20, -20, -10, //
    20, 20, 0, 0, 0, 0, 20, 20, //
    20, 30, 10, 0, 0, 10, 30, 20,
];

pub const OPTIMAL_SQUARES: [[i32; 64]; 6] = [
    KNIGHT_SQUARES,
    BISHOP_SQUARES,
    ROOK_SQUARES,
    QUEEN_SQUARES,
    PAWN_SQUARES,
    KING_SQUARES,
];

pub const PIECE_VALUES: [i32; 6] = [320, 330, 500, 900, 100, 20000];

const fn sliding_moves() -> [[u64; 8]; 64] {
    let mut moves = [[0u64; 8]; 64];

    let shifts: [(isize, isize); 8] = [
        (-1, 1),  // NW
        (0, 1),  // North
        (1, 1), // NE
        (1, 0), // East
        (1, -1),// SE
        (0, -1), // South
        (-1, -1), // SW
        (-1, 0),  // West
    ];

    let mut square = 0;
    while square < 64 {
        let mut idx = 0;
        while idx < shifts.len() {
            let (dx, dy) = shifts[idx];

            let mut x = (square % 8) as isize;
            let mut y = (square / 8) as isize;

            loop {
                x += dx;
                y += dy;

                if x < 0 || x >= 8 || y < 0 || y >= 8 {
                    break;
                }

                let target_square = (y * 8 + x) as usize;
                moves[square][idx] |= 1 << target_square;
            }

            idx += 1;
        }

        square += 1;
    }

    moves
}

/// NW, North, NE, East, SE, South, SW, West
pub const SLIDING_MOVES: [[u64; 8]; 64] = sliding_moves();
