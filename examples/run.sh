cd ..
cargo run examples/$1 examples/out.asm
cd examples/
nasm -f elf64 out.asm -F Dwarf
ld out.o
./a.out