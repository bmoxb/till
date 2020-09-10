use crate::checking;
use super::Generator;
use std::fmt;

pub fn input(instructions: Vec<checking::Instruction>) -> String {
    GenerateNasm::new().execute(instructions).to_string()
}

struct GenerateNasm {
    text_section: String,
    data_section: String,
    rodata_section: String,
    num_label_counter: usize
}

impl GenerateNasm {
    fn new() -> Self {
        GenerateNasm {
            text_section: INITIAL_TEXT.to_string(),
            data_section: INITIAL_DATA.to_string(),
            rodata_section: INITIAL_RODATA.to_string(),
            num_label_counter: 0
        }
    }
}

const INITIAL_TEXT: &'static str = "
section .text
global _start
_start:
";

const INITIAL_DATA: &'static str = "\nsection .data\n";

const INITIAL_RODATA: &'static str = "\nsection .rodata\n";

const ADD_INSTRUCTIONS: &'static str = "
fld qword [rsp] ; Load top of stack onto FPU stack
fld qword [rsp + 8] ; Load second-to-top of stack onto FPU stack
fadd ; Perform operation
add rsp, 8 ; Move stack pointer
fst qword [rsp] ; Store result on stack
";

const EXIT_INSTRUCTIONS: &'static str = "
mov rax, 60 ; Exit syscall (sys_exit)
mov rdi, 0 ; Ok error code
syscall ; Perform the syscall operation
";

impl Generator for GenerateNasm {
    const TARGET_NAME: &'static str = "Linux elf64";

    fn handle_instruction(&mut self, instruction: checking::Instruction) {
        match instruction {
            checking::Instruction::Push(checking::Value::Num(num_val)) => {
                let label = format!("num{}", self.num_label_counter);
                self.num_label_counter += 1;

                self.rodata_section += &store_under_label(&label, num_val, "Store number literal");
                self.text_section += &push_address(&label, "Push number literal stored in .rodata section");
            }

            checking::Instruction::Label(id) => { self.text_section += &format!("label{}:\n", id); }

            checking::Instruction::Jump(id) => { self.text_section += &format!("jmp label{}\n", id); }

            checking::Instruction::Add => { self.text_section += ADD_INSTRUCTIONS; }

            _ => {}
        }
    }

    fn construct_output(mut self) -> String {
        self.text_section.push_str(EXIT_INSTRUCTIONS);
        format!("{}\n{}\n{}", self.text_section, self.data_section, self.rodata_section)
    }
}

fn store_under_label<T: fmt::Display>(label: &str, val: T, comment: &str) -> String { format!("{}: rq {} ; {}\n", label, val, comment) }

fn push_address<T: fmt::Display>(val: &T, comment: &str) -> String { format!("push [{}] ; {}\n", val, comment) }