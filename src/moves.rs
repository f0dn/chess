use crate::consts::*;
use std::fmt::Display;

pub const DOUBLE_PAWN_PUSH: u16 = 0b0011;
pub const KING_CASTLE: u16 = 0b0010;
pub const QUEEN_CASTLE: u16 = 0b0001;
pub const EN_PASSANT: u16 = 0b0101;

pub const PROMOTION: u16 = 0b1000;
pub const CAPTURE: u16 = 0b0100;

#[derive(Copy, Clone)]
pub struct Move {
    info: u16,
}

impl Move {
    pub fn new(from: u16, to: u16, flags: u16) -> Self {
        Move {
            info: (from & 0x3F) | ((to & 0x3F) << 6) | ((flags & 0xF) << 12),
        }
    }

    pub fn from_uci(uci: &str) -> Option<Self> {
        if uci.len() < 4 {
            return None;
        }

        let bytes = uci.as_bytes();

        let from_file = (bytes[0] - b'a') as u16;
        let from_rank = (bytes[1] - b'1') as u16;
        let to_file = (bytes[2] - b'a') as u16;
        let to_rank = (bytes[3] - b'1') as u16;

        if bytes.len() > 4 {
            // Handle promotion
            let promotion_piece = bytes[4] as char;
            let flags = match promotion_piece {
                'q' | 'Q' => PROMOTION | QUEEN as u16,
                'r' | 'R' => PROMOTION | ROOK as u16,
                'b' | 'B' => PROMOTION | BISHOP as u16,
                'n' | 'N' => PROMOTION | KNIGHT as u16,
                _ => return None,
            };

            let from = from_rank * 8 + from_file;
            let to = to_rank * 8 + to_file;

            return Some(Move::new(from, to, flags));
        }

        let from = from_rank * 8 + from_file;
        let to = to_rank * 8 + to_file;

        Some(Move::new(from, to, 0))
    }

    pub fn append_flags(&mut self, flags: u16) {
        self.info |= (flags & 0xF) << 12;
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
        let promotion_piece = if self.flags() & PROMOTION != 0 {
            match (self.flags() & 0b11) as usize {
                QUEEN => "q",
                ROOK => "r",
                BISHOP => "b",
                KNIGHT => "n",
                _ => "",
            }
        } else {
            ""
        };

        write!(
            f,
            "{}{}{}{}{}",
            (b'a' + (self.from() % 8) as u8) as char,
            (b'1' + (self.from() / 8) as u8) as char,
            (b'a' + (self.to() % 8) as u8) as char,
            (b'1' + (self.to() / 8) as u8) as char,
            promotion_piece
        )
    }
}
