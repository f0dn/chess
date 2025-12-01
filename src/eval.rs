use std::{fmt::Display, ops::Neg};

const CHECKMATE: i32 = 1_000_000;
pub const DRAW_EVAL: Eval = Eval { eval: 0 };
pub const MAX_EVAL: Eval = Eval { eval: i32::MAX };
pub const MIN_EVAL: Eval = Eval { eval: i32::MIN };

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Eval {
    eval: i32,
}

impl Eval {
    pub fn new(eval: i32) -> Self {
        Self { eval }
    }

    pub fn mate_in(steps: i32) -> Self {
        Self {
            eval: if steps >= 0 {
                CHECKMATE + steps
            } else {
                -CHECKMATE + steps
            },
        }
    }
}

impl Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.eval.abs() >= CHECKMATE {
            let steps = if self.eval > 0 {
                self.eval - CHECKMATE
            } else {
                self.eval + CHECKMATE
            };
            write!(f, "mate {}", steps)
        } else {
            write!(f, "cp {}", self.eval)
        }
    }
}

impl Neg for Eval {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self { eval: -self.eval }
    }
}
