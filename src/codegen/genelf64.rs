//! Module contain code for the generation of x86_64 elf64 Intel-syntax assembly
//! code.

use crate::checking;
use super::Generator;
use std::collections::HashMap;

pub fn input(instructions: Vec<checking::Instruction>) -> String {
    GenerateElf64::new().execute(instructions)
}

struct GenerateElf64 {
    text_section: Vec<Instruction>,
    rodata_section: Vec<Instruction>,
    num_label_counter: usize,
    function_variable_locations: HashMap<checking::Id, Oprand>,
    local_variable_num: usize,
    parameter_variable_num: usize,
    display_num_used: bool,
    display_bool_used: bool,
    display_char_used: bool
}

impl GenerateElf64 {
    fn new() -> Self {
        GenerateElf64 {
            text_section: vec![
                Instruction::Comment(format!("Target: {}", Self::TARGET_NAME)),
                Instruction::Section("text".to_string()),
                Instruction::Extern("printf".to_string()),
                Instruction::Global("main".to_string())
            ],
            rodata_section: vec![Instruction::Section("rodata".to_string())],
            num_label_counter: 0,
            function_variable_locations: HashMap::new(),
            local_variable_num: 0,
            parameter_variable_num: 0,
            display_num_used: false,
            display_bool_used: false,
            display_char_used: false
        }
    }
}

const BYTES_IN_VALUE: usize = 8;
const CARRY_FLAG_BIT_OFFSET: usize = 8;
const ZERO_FLAG_BIT_OFFSET: usize = 14;

const POP_AND_CMP_WITH_ZERO_INSTRUCTIONS: &'static [Instruction] = &[
    Instruction::Pop(Oprand::Register(Reg::Rax)),
    Instruction::Cmp { dest: Oprand::Register(Reg::Rax), src: Oprand::Value(Val::Int(0)) }
];

impl Generator for GenerateElf64 {
    const TARGET_NAME: &'static str = "Linux elf64";

    fn handle_instruction(&mut self, instruction: checking::Instruction) {
        self.text_section.push(Instruction::Comment(format!("{:?}", instruction)));
        match instruction {
            checking::Instruction::Push(val) => {
                let oprand = match val {
                    checking::Value::Num(num_val) => {
                        let label = literal_label(self.num_label_counter);
                        self.num_label_counter += 1;

                        self.rodata_section.extend(vec![
                            Instruction::Label(label.clone()),
                            Instruction::Declare(Val::Float(num_val))
                        ]);

                        Oprand::Address(Box::new(Oprand::Label(label)))
                    }

                    checking::Value::Variable(var_id) =>
                        self.function_variable_locations.get(&var_id).unwrap().clone(),

                    checking::Value::Char(chr_val) =>
                        Oprand::Value(Val::Int(chr_val as isize)),

                    checking::Value::Bool(bool_val) =>
                        Oprand::Value(Val::Int(if bool_val { 1 } else { 0 }))
                };

                self.text_section.push(Instruction::Push(oprand));
            }

            checking::Instruction::Store(id) => {
                let location = self.function_variable_locations.get(&id).unwrap();

                self.text_section.push(Instruction::Pop(location.clone()));
            }

            checking::Instruction::Parameter(id) => {
                self.function_variable_locations.insert(
                    id,
                    Oprand::AddressDisplaced(
                        Box::new(Oprand::Register(Reg::BasePointer)),
                        ((self.parameter_variable_num + 2) * BYTES_IN_VALUE) as isize
                    )
                );

                self.parameter_variable_num += 1;
            }

            checking::Instruction::Local(id) => {
                self.function_variable_locations.insert(
                    id,
                    Oprand::AddressDisplaced(
                        Box::new(Oprand::Register(Reg::BasePointer)),
                        -(BYTES_IN_VALUE as isize) * (self.local_variable_num as isize + 1)
                    )
                );

                self.local_variable_num += 1;
            }

            checking::Instruction::Label(id) => { self.text_section.push(Instruction::Label(label(id))); }

            checking::Instruction::Function { label, local_variable_count } => {
                // Beginning a new function so naturally there are no local
                // variables or parameters defined yet:
                self.local_variable_num = 0;
                self.parameter_variable_num = 0;
                self.function_variable_locations.clear();

                self.text_section.extend(vec![
                    Instruction::Label(label),
                    // Preserve the base pointer of the previous frame:
                    Instruction::Push(Oprand::Register(Reg::BasePointer)),
                    // Create a new frame beginning at the current stack top:
                    Instruction::Mov {
                        dest: Oprand::Register(Reg::BasePointer),
                        src: Oprand::Register(Reg::StackPointer)
                    },
                    // Reserve stack space for the storage of local variables:
                    Instruction::Sub {
                        dest: Oprand::Register(Reg::StackPointer),
                        src: Oprand::Value(Val::Int((local_variable_count * BYTES_IN_VALUE) as isize))
                    }
                ]);
            }

            checking::Instruction::CallExpectingVoid(label) => { self.text_section.push(Instruction::Call(label)); }

            checking::Instruction::CallExpectingValue(label) => {
                self.text_section.extend(vec![
                    Instruction::Call(label),
                    // Place the function return value on the stack:
                    Instruction::Push(Oprand::Register(Reg::Rax))
                ]);
            }

            checking::Instruction::ReturnVoid => self.add_return_instructions(),

            checking::Instruction::ReturnValue => {
                // Place function return value in register:
                self.text_section.push(Instruction::Pop(Oprand::Register(Reg::Rax)));
                self.add_return_instructions();
            }

            checking::Instruction::Display { value_type, line_number } => {
                let (format_label, float_args_count) = match value_type {
                    checking::Type::Char => {
                        self.display_char_used = true;
                        // Pop character from stack into rdx (third argument):
                        self.text_section.push(Instruction::Pop(Oprand::Register(Reg::Rdx)));
                        ("display_char", 0)
                    }
                    checking::Type::Bool => {
                        self.display_bool_used = true;
                        // Pop bool from stack into rdx (third argument):
                        self.text_section.push(Instruction::Pop(Oprand::Register(Reg::Rdx)));
                        ("display_bool", 0)
                    }
                    checking::Type::Num => {
                        self.display_num_used = true;
                        // Pop and store float in xmm0 register (first floating-point argument):
                        self.text_section.extend(vec![
                            Instruction::Pop(Oprand::Register(Reg::Rax)),
                            Instruction::Movq {
                                dest: Oprand::Register(Reg::Xmm0),
                                src: Oprand::Register(Reg::Rax)
                            }
                        ]);
                        ("display_num", 1)
                    }
                };

                self.text_section.extend(vec![
                    // Load format string (first argument):
                    Instruction::Mov { dest: Oprand::Register(Reg::DestIndex), src: Oprand::Label(format_label.to_string()) },
                    // Load line number (second argument):
                    Instruction::Mov { dest: Oprand::Register(Reg::SrcIndex), src: Oprand::Value(Val::Int(line_number as isize)) },
                    // Indicate number of floating-point arguments:
                    Instruction::Mov { dest: Oprand::Register(Reg::Rax), src: Oprand::Value(Val::Int(float_args_count)) },
                    // Preserve stack pointer:
                    Instruction::Mov { dest: Oprand::Register(Reg::Rbx), src: Oprand::Register(Reg::StackPointer) },
                    // Align stack to 16-byte boundary:
                    Instruction::BitwiseAnd { dest: Oprand::Register(Reg::StackPointer), src: Oprand::Value(Val::Int(-16)) },
                    // Call printf function:
                    Instruction::Call("printf".to_string()),
                    // Restore stack pointer:
                    Instruction::Mov { dest: Oprand::Register(Reg::StackPointer), src: Oprand::Register(Reg::Rbx) }
                ]);
            }

            checking::Instruction::Jump(id) => { self.text_section.push(Instruction::Jmp(label(id))); }

            checking::Instruction::JumpIfTrue(id) => {
                self.text_section.extend_from_slice(POP_AND_CMP_WITH_ZERO_INSTRUCTIONS);
                // Jump if top of stack not equal to 0:
                self.text_section.push(Instruction::Jne(label(id)));
            }

            checking::Instruction::JumpIfFalse(id) => {
                self.text_section.extend_from_slice(POP_AND_CMP_WITH_ZERO_INSTRUCTIONS);
                // Jump if top of stack equals 0:
                self.text_section.push(Instruction::Je(label(id)));
            }

            checking::Instruction::Equals => {
                self.text_section.extend(vec![
                    // Take first value in comparison off the stack:
                    Instruction::Pop(Oprand::Register(Reg::Rax)),
                    // Subtract that value by the second top value on stack:
                    Instruction::Sub {
                        dest: Oprand::Register(Reg::Rax),
                        src: Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer)))
                    },
                    // Push flags register onto the stack:
                    Instruction::PushFlags,
                    // Pop the flags register into rax:
                    Instruction::Pop(Oprand::Register(Reg::Rax)),
                    // Extract the value of the zero flag:
                    Instruction::Shr { dest: Oprand::Register(Reg::Rax), shift_by: 6 },
                    Instruction::BitwiseAnd { dest: Oprand::Register(Reg::Rax), src: Oprand::Value(Val::Int(1)) },
                    // Place the value of the zero flag onto the stack:
                    Instruction::Mov {
                        dest: Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer))),
                        src: Oprand::Register(Reg::Rax)
                    }
                ]);
            }

            checking::Instruction::Add => self.add_arithmetic_instructions(Instruction::FpuAdd),
            checking::Instruction::Subtract => self.add_arithmetic_instructions(Instruction::FpuSubtract),
            checking::Instruction::Multiply => self.add_arithmetic_instructions(Instruction::FpuMultiply),
            checking::Instruction::Divide => self.add_arithmetic_instructions(Instruction::FpuDivide),

            checking::Instruction::GreaterThan => {
                self.add_comparison_instructions(vec![
                    // Extract the carry flag bit (indicates greater than when set in this instance):
                    Instruction::Shr { dest: Oprand::Register(Reg::Ax), shift_by: CARRY_FLAG_BIT_OFFSET }
                ]);
            }

            checking::Instruction::LessThan => {
                self.add_comparison_instructions(vec![
                    // Create second copy of FPU status word:
                    Instruction::Mov { dest: Oprand::Register(Reg::Bx), src: Oprand::Register(Reg::Ax) },
                    // Have carry flag as least significant bit of ax:
                    Instruction::Shr { dest: Oprand::Register(Reg::Ax), shift_by: CARRY_FLAG_BIT_OFFSET },
                    // Have zero flag as least significant bit of bx:
                    Instruction::Shr { dest: Oprand::Register(Reg::Bx), shift_by: ZERO_FLAG_BIT_OFFSET },
                    // Both carry flag and zero flag being 0 indicates less than:
                    Instruction::BitwiseOr { dest: Oprand::Register(Reg::Ax), src: Oprand::Register(Reg::Bx) },
                    Instruction::BitwiseNot(Oprand::Register(Reg::Ax))
                ]);
            }

            checking::Instruction::Not => {
                self.text_section.extend(vec![
                    // Perform bitwise not on value on top of stack:
                    Instruction::BitwiseNot(Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer)))),
                    // Discard all bits except the least significant:
                    Instruction::BitwiseAnd {
                        dest: Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer))),
                        src: Oprand::Value(Val::Int(1))
                    }
                ]);
            }
        }
    }

    fn construct_output(mut self) -> String {
        if self.display_char_used {
            self.rodata_section.extend(vec![
                Instruction::Label("display_char".to_string()),
                Instruction::DeclareString(r"Line %u character value: '%c'\n\0".to_string())
            ]);
        }

        if self.display_bool_used {
            self.rodata_section.extend(vec![
                Instruction::Label("display_bool".to_string()),
                Instruction::DeclareString(r"Line %u boolean value: %lld\n\0".to_string())
            ]);
        }

        if self.display_num_used {
            self.rodata_section.extend(vec![
                Instruction::Label("display_num".to_string()),
                Instruction::DeclareString(r"Line %u number value: %f\n\0".to_string())
            ]);
        }

        self.text_section.extend(self.rodata_section.into_iter());

        self.text_section.into_iter().map(|x| x.intel_syntax()).collect::<Vec<String>>().join("")
    }
}

impl GenerateElf64 {
    fn two_stack_items_to_fpu_stack(&mut self, operation: Instruction) {
        self.text_section.extend(vec![
            Instruction::FpuReset,
            // Load second-to-top of stack onto FPU stack:
            Instruction::FpuPush(Oprand::AddressDisplaced(Box::new(Oprand::Register(Reg::StackPointer)), BYTES_IN_VALUE as isize)),
            // Load top of stack onto FPU stack:
            Instruction::FpuPush(Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer)))),
            // Perform the given operation:
            operation,
            // Move stack pointer:
            Instruction::Add { dest: Oprand::Register(Reg::StackPointer), src: Oprand::Value(Val::Int(BYTES_IN_VALUE as isize)) },
        ]);
    }

    fn add_arithmetic_instructions(&mut self, operation: Instruction) {
        self.two_stack_items_to_fpu_stack(operation);

        self.text_section.push( // Move result from FPU stack to regular stack:
            Instruction::FpuPop(Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer)))),
        );
    }
    
    fn add_comparison_instructions(&mut self, operations: Vec<Instruction>) {
        self.two_stack_items_to_fpu_stack(Instruction::FpuCompare);
       
        self.text_section.push( // Store the FPU status register in ax:
            Instruction::FpuStatusReg(Oprand::Register(Reg::Ax))
        );
        
        self.text_section.extend(operations);
        
        self.text_section.extend(vec![
            // Ensure all bits except the least significant one are clear:
            Instruction::BitwiseAnd { dest: Oprand::Register(Reg::Rax), src: Oprand::Value(Val::Int(1)) },
            //  Store result:
            Instruction::Mov {
                dest: Oprand::Address(Box::new(Oprand::Register(Reg::StackPointer))),
                src: Oprand::Register(Reg::Rax)
            }
        ]);
    }

    fn add_return_instructions(&mut self) {
        self.text_section.extend(vec![
            // Restore stack pointer:
            Instruction::Mov {
                dest: Oprand::Register(Reg::StackPointer),
                src: Oprand::Register(Reg::BasePointer)
            },
            // Restore the base pointer of the previous frame:
            Instruction::Pop(Oprand::Register(Reg::BasePointer)),
            // Remove parameter values from the stack and return:
            Instruction::Ret(self.parameter_variable_num * BYTES_IN_VALUE)
        ]);
    }
}

/// Trait for conversion to Intel or AT&T assembly syntax.
trait AssemblyDisplay {
    fn intel_syntax(self) -> String;
    fn at_and_t_syntax(self) -> String where Self: Sized { unimplemented!() }
}

#[derive(Clone)]
enum Instruction {
    Comment(String),
    Section(String),
    Extern(String),
    Global(String),
    Label(String),
    Declare(Val),
    DeclareString(String),
    Mov { dest: Oprand, src: Oprand },
    Movq { dest: Oprand, src: Oprand },
    Add { dest: Oprand, src: Oprand },
    Sub { dest: Oprand, src: Oprand },
    Push(Oprand),
    Pop(Oprand),
    FpuPush(Oprand),
    FpuPop(Oprand),
    FpuStatusReg(Oprand),
    FpuReset,
    FpuCompare,
    FpuAdd,
    FpuSubtract,
    FpuMultiply,
    FpuDivide,
    Ret(usize),
    Call(String),
    Jmp(String),
    Shr { dest: Oprand, shift_by: usize },
    BitwiseAnd { dest: Oprand, src: Oprand },
    BitwiseOr { dest: Oprand, src: Oprand },
    BitwiseNot(Oprand),
    PushFlags,
    Cmp { dest: Oprand, src: Oprand },
    Je(String),
    Jne(String)
}

impl AssemblyDisplay for Instruction {
    fn intel_syntax(self) -> String {
        match self {
            Instruction::Comment(x) => format!("; {}\n", x),
            Instruction::Section(x) => format!("section .{}\n", x),
            Instruction::Extern(x) => format!("extern {}\n", x),
            Instruction::Global(x) => format!("global {}\n", x),
            Instruction::Label(x) => format!("{}:\n", x),
            Instruction::Declare(x) => format!("dq {}\n", x.intel_syntax()),
            Instruction::DeclareString(x) => format!("db `{}`\n", x),
            Instruction::Mov { dest, src } => format!("mov {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::Movq { dest, src } => format!("movq {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::Add { dest, src } => format!("add {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::Sub { dest, src } => format!("sub {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::Push(x) => format!("push qword {}\n", x.intel_syntax()),
            Instruction::Pop(x) => format!("pop qword {}\n", x.intel_syntax()),
            Instruction::FpuPush(x) => format!("fld qword {}\n", x.intel_syntax()),
            Instruction::FpuPop(x) => format!("fst qword {}\n", x.intel_syntax()),
            Instruction::FpuStatusReg(x) => format!("fstsw {}\n", x.intel_syntax()),
            Instruction::FpuReset => "finit\n".to_string(),
            Instruction::FpuCompare => "fcom\n".to_string(),
            Instruction::FpuAdd => "fadd\n".to_string(),
            Instruction::FpuSubtract => "fsub\n".to_string(),
            Instruction::FpuMultiply => "fmul\n".to_string(),
            Instruction::FpuDivide => "fdiv\n".to_string(),
            Instruction::Ret(x) => format!("ret {}\n", x),
            Instruction::Call(x) => format!("call {}\n", x),
            Instruction::Jmp(x) => format!("jmp {}\n", x),
            Instruction::Shr { dest, shift_by } => format!("shr {}, {}\n", dest.intel_syntax(), shift_by),
            Instruction::BitwiseAnd { dest, src } => format!("and qword {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::BitwiseOr { dest, src } => format!("or qword {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::BitwiseNot(x) => format!("not qword {}\n", x.intel_syntax()),
            Instruction::PushFlags => "pushfq\n".to_string(),
            Instruction::Cmp { dest, src } => format!("cmp {}, {}\n", dest.intel_syntax(), src.intel_syntax()),
            Instruction::Je(x) => format!("je {}\n", x),
            Instruction::Jne(x) => format!("jne {}\n", x)
        }
    }
}

#[derive(Clone)]
enum Oprand {
    Label(String),
    Value(Val),
    Register(Reg),
    Address(Box<Oprand>),
    AddressDisplaced(Box<Oprand>, isize),
}

impl AssemblyDisplay for Oprand {
    fn intel_syntax(self) -> String {
        match self {
            Oprand::Label(x) => x,
            Oprand::Value(x) => x.intel_syntax(),
            Oprand::Register(x) => x.intel_syntax(),
            Oprand::Address(x) => format!("[{}]", x.intel_syntax()),
            Oprand::AddressDisplaced(x, displacement) => format!("[{}{:+}]", x.intel_syntax(), displacement)
        }
    }
}

#[derive(Clone)]
enum Val { Int(isize), Float(f64) }

impl AssemblyDisplay for Val {
    fn intel_syntax(self) -> String {
        match self {
            Val::Int(x) => x.to_string(),
            Val::Float(x) => format!("{:.16}", x)
        }
    }
}

#[derive(Clone)]
enum Reg { Rax, Ax, Rbx, Bx, Rdx, StackPointer, BasePointer, DestIndex, SrcIndex, Xmm0 }

impl AssemblyDisplay for Reg {
    fn intel_syntax(self) -> String {
        match self {
            Reg::Rax => "rax",
            Reg::Ax => "ax",
            Reg::Rbx => "rbx",
            Reg::Bx => "bx",
            Reg::Rdx => "rdx",
            Reg::StackPointer => "rsp",
            Reg::BasePointer => "rbp",
            Reg::DestIndex => "rdi",
            Reg::SrcIndex => "rsi",
            Reg::Xmm0 => "xmm0"
        }.to_string()
    }
}

fn label(id: usize) -> String { format!("label{}", id) }

fn literal_label(counter: usize) -> String { format!("literal{}", counter) }