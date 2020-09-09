use crate::checking;

pub mod genx86;

trait Generator {
    fn input(instructions: Vec<checking::Instruction>) -> Vec<u8>;
}