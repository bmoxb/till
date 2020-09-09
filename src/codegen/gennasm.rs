use crate::checking;
use super::Generator;
use std::fmt;

pub fn input(instructions: Vec<checking::Instruction>) -> String {
    GenerateNasm::new().execute(instructions).to_string()
}

enum Instruction {
    Section(String),
    Global(String),
    Label(String),
    Push(Value),
    Pop(Value),
    Define(f64),
    FpuPush(Value),
    FpuPop(Value),
    FpuAdd
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Section(x) => writeln!(f, "section .{}", x),
            Instruction::Global(x) => writeln!(f, "global {}", x),
            Instruction::Label(x) => writeln!(f, "{}:", x),
            Instruction::Push(x) => writeln!(f, "push qword {}", x),
            Instruction::Pop(x) => writeln!(f, "pop qword {}", x),
            Instruction::Define(x) => writeln!(f, "dq {}", x),
            Instruction::FpuPush(x) => writeln!(f, "fld qword {}", x),
            Instruction::FpuPop(x) => writeln!(f, "fst qword {}", x),
            Instruction::FpuAdd => writeln!(f, "fadd")
        }
    }
}

enum Value {
    Number(f64),
    Character(char),
    Register(Reg),
    Label(String),
    AddressOf(Box<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{:.16}", x),
            Value::Character(x) => write!(f, "'{}'", x),
            Value::Register(x) => write!(f, "{}", x),
            Value::Label(x) => write!(f, "{}", x),
            Value::AddressOf(x) => write!(f, "[{}]", x),
        }
    }
}

enum Reg { Rax, StackPointer }

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::Rax => write!(f, "rax"),
            Reg::StackPointer => write!(f, "asp")
        }
    }
}

struct GenerateNasm {
    text_section: Vec<Instruction>,
    data_section: Vec<Instruction>,
    rodata_section: Vec<Instruction>,
    num_label_counter: usize
}

impl GenerateNasm {
    fn new() -> Self {
        GenerateNasm {
            text_section: vec![
                Instruction::Section("text".to_string()),
                Instruction::Global("_start".to_string()),
                Instruction::Label("_start".to_string())
            ],
            data_section: vec![ Instruction::Section("data".to_string()) ],
            rodata_section: vec![ Instruction::Section("rodata".to_string()) ],
            num_label_counter: 0
        }
    }

    fn pop_into_fpu_stack(&mut self) {
        self.text_section.extend(vec![
            Instruction::Pop(Value::Register(Reg::Rax)),
            Instruction::FpuPush(Value::Register(Reg::Rax)) // TODO: Use stack pointer displacement then move stack pointer to remove the used values.
        ]);
    }

    fn push_from_fpu_stack(&mut self) {
        self.text_section.push(Instruction::FpuPop(
            Value::AddressOf(Box::new(Value::Register(Reg::StackPointer))) // TODO: Needs displacement too...
        ));
    }
}

impl Generator for GenerateNasm {
    const TARGET_NAME: &'static str = "x86";

    fn handle_instruction(&mut self, instruction: checking::Instruction) {
        match instruction {
            checking::Instruction::Push(checking::Value::Bool(bool_val)) => {
                self.text_section.push(Instruction::Push(
                    Value::Number(if bool_val { 1.0 } else { 0.0 })
                ));
            }

            checking::Instruction::Push(checking::Value::Char(char_val)) => {
                self.text_section.push(Instruction::Push(
                    Value::Character(char_val)
                ));
            }

            checking::Instruction::Push(checking::Value::Num(num_val)) => {
                let label = format!("num{}", self.num_label_counter);
                
                self.num_label_counter += 1;

                self.rodata_section.extend(vec![
                    Instruction::Label(label.clone()),
                    Instruction::Define(num_val)
                ]);

                self.text_section.push(Instruction::Push(
                    Value::AddressOf(Box::new(Value::Label(label)))
                ));
            }

            checking::Instruction::Add => {
                self.pop_into_fpu_stack();
                self.pop_into_fpu_stack();
                self.text_section.push(Instruction::FpuAdd);
                self.push_from_fpu_stack();
            }

            _ => {} // TODO
        }
    }

    fn construct_output(mut self) -> String {
        self.text_section.extend(self.data_section);
        self.text_section.extend(self.rodata_section);
        self.text_section.iter().map(Instruction::to_string).collect()
    }
}