use std::fmt::Display;

pub const DOUBLE_PAWN_PUSH: u16 = 0b0001;
pub const KING_CASTLE: u16 = 0b0010;
pub const QUEEN_CASTLE: u16 = 0b0011;
pub const EN_PASSANT: u16 = 0b0101;

pub const PROMOTION: u16 = 0b1000;
pub const CAPTURE: u16 = 0b0100;

pub struct Move {
    info: u16,
}

impl Move {
    pub fn new(from: u16, to: u16, flags: u16) -> Self {
        Move {
            info: (from & 0x3F) | ((to & 0x3F) << 6) | ((flags & 0xF) << 12),
        }
    }

    pub fn from(&self) -> u16 {
        self.info & 0x3F
    }

    pub fn to(&self) -> u16 {
        (self.info >> 6) & 0x3F
    }

    pub fn flags(&self) -> u16 {
        (self.info >> 12) & 0xF
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            (b'a' + (self.from() % 8) as u8) as char,
            (b'1' + (self.from() / 8) as u8) as char,
            (b'a' + (self.to() % 8) as u8) as char,
            (b'1' + (self.to() / 8) as u8) as char
        )
    }
}
