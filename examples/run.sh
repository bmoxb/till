cd ..
cargo run examples/$1 examples/out.asm
cd examples/
nasm -f elf64 out.asm -F Dwarf
gcc out.o -o a.out
./a.out
