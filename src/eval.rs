use std::{
    fmt::{Display, Formatter, Result},
    ops::Neg,
};

const CHECKMATE: i32 = 1_000_000;
const CHECKMATE_BOUND: i32 = 900_000;
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
                CHECKMATE - steps
            } else {
                -(CHECKMATE - steps)
            },
        }
    }

    pub fn add_mate_steps(&self, steps: i32) -> Self {
        if let Some(s) = self.steps_to_mate() {
            Self::mate_in(s + steps)
        } else {
            *self
        }
    }

    pub fn steps_to_mate(&self) -> Option<i32> {
        if self.eval.abs() >= CHECKMATE_BOUND {
            Some(if self.eval > 0 {
                CHECKMATE - self.eval
            } else {
                -(CHECKMATE + self.eval)
            })
        } else {
            None
        }
    }
}

impl Display for Eval {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(steps) = self.steps_to_mate() {
            write!(f, "mate {}", (steps - 1) / 2)
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
