//! Contains code for generating assembly code based upon a till program final
//! immediate representation.

pub mod genelf64;

use crate::checking;

/// Generate assembly code from final IR instructions trait.
trait Generator {
    const TARGET_NAME: &'static str;

    /// Convert a set of given final immediate representation instructions into
    /// assembly code.
    fn execute(mut self, instructions: Vec<checking::Instruction>) -> String where Self: Sized {
        for instruction in instructions {
            log::trace!("Handling instruction: {:?}", instruction);

            self.handle_instruction(instruction);
        }

        self.construct_output()
    }

    fn handle_instruction(&mut self, instruction: checking::Instruction);

    fn construct_output(self) -> String;
}