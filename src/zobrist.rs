const fn random(seed: u64) -> u64 {
    let mut z = seed;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^ (z >> 31)
}

pub const ZOBRIST_TABLE: [[u64; 12]; 64] = {
    let mut table = [[0u64; 12]; 64];
    let mut i = 0;
    while i < 64 {
        let mut j = 0;
        while j < 12 {
            table[i][j] = random((i as u64) * 12 + (j as u64) + 1);
            j += 1;
        }
        i += 1;
    }
    table
};

pub const ZOBRIST_BLACK_TO_MOVE: u64 = random(773);
pub const ZOBRIST_CASTLING_RIGHTS: [u64; 16] = {
    let mut rights = [0u64; 16];
    let mut i = 0;
    while i < 16 {
        rights[i] = random(1000 + (i as u64));
        i += 1;
    }
    rights
};
