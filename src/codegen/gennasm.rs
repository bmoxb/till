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

const INITIAL_TEXT: &'static str = "\
section .text
global _start
_start:
";

const INITIAL_DATA: &'static str = "section .data\n";

const INITIAL_RODATA: &'static str = "section .rodata\n";

const ADD_INSTRUCTIONS: &'static str = "\
fld qword [rsp] ; Load top of stack onto FPU stack
fld qword [rsp + 8] ; Load second-to-top of stack onto FPU stack
fadd ; Perform operation
add rsp, 8 ; Move stack pointer
fst qword [rsp] ; Store result on stack
";

const FUNCTION_INSTRUCTION: &'static str = "\
push rbp ; Preserve the base pointer of the previous frame
mov rbp, rsp ; Create a new frame beginning at the current stack top
";

const AFTER_CALL_INSTRUCTION: &'static str = "push rax ; Place the function return value on the stack\n";

const RETURN_INSTRUCTIONS: &'static str = "\
pop rbp ; Restore the base pointer of the previous frame
ret 16 ; Shift stack pointer by 2 (remove old base pointer, return address) when returning
";

const EXIT_INSTRUCTIONS: &'static str = "\
mov rax, 60 ; Exit syscall (sys_exit)
mov rdi, 0 ; Ok error code
syscall ; Perform the syscall operation
";

impl Generator for GenerateNasm {
    const TARGET_NAME: &'static str = "Linux elf64";

    fn handle_instruction(&mut self, instruction: checking::Instruction) {
        match instruction {
            checking::Instruction::Allocate(id) => { self.data_section += &store_num_under_label(&var_label(id), 0); }

            checking::Instruction::Push(val) => {
                match val {
                    checking::Value::Num(num_val) => {
                        let label = literal_label(self.num_label_counter);
                        self.num_label_counter += 1;

                        self.rodata_section += &store_num_under_label(&label, num_val);
                        self.text_section += &push_address(&label, "Push number literal stored in .rodata section");
                    }

                    checking::Value::Variable(var_id) => {
                        self.text_section += &push_address(&var_label(var_id), "Push variable stored in the .data section");
                    }

                    _ => unimplemented!()
                }
            }

            checking::Instruction::Store(id) => { self.text_section += &pop_address(&var_label(id), "Store in .data section"); }

            checking::Instruction::Parameter { store_in, param_number } => {
                self.text_section += &format!("mov rbx, [rbp + {}] ; {}\nmov [{}], rbx\n",
                    16 + (param_number * 8),
                    "Store function argument in parameter variable",
                    &var_label(store_in)
                );
            }

            checking::Instruction::Label(id) => { self.text_section += &format!("{}:\n", label(id)); }

            checking::Instruction::Function(id) => { self.text_section += &format!("{}:\n{}", func_label(id), FUNCTION_INSTRUCTION); }

            checking::Instruction::CallExpectingVoid(id) => { self.text_section += &format!("call {}\n", func_label(id)); }

            checking::Instruction::CallExpectingValue(id) => {
                self.text_section += &format!("call {}\n{}", func_label(id), AFTER_CALL_INSTRUCTION);
            }

            checking::Instruction::ReturnVoid => { self.text_section += RETURN_INSTRUCTIONS; }

            checking::Instruction::ReturnValue => {
                self.text_section += "pop rax ; Place function return value in register\n";
                self.text_section += RETURN_INSTRUCTIONS;
            }

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

fn label(id: usize) -> String { format!("label{}", id) }

fn func_label(id: usize) -> String { format!("func{}", id) }

fn var_label(id: usize) -> String { format!("var{}", id) }

fn literal_label(counter: usize) -> String { format!("literal{}", counter) }

fn store_num_under_label<T: fmt::Display>(label: &str, val: T) -> String { format!("{}: dq {:.16}\n", label, val) }

fn push_address(val: &str, comment: &str) -> String { format!("push qword [{}] ; {}\n", val, comment) }

fn pop_address(val: &str, comment: &str) -> String { format!("pop qword [{}] ; {}\n", val, comment) }