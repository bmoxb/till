#!/bin/bash
set -x

cat $1
cd ..
cargo run examples/$1 examples/out.asm
cd examples/
nasm out.asm -f elf64 -F Dwarf -w-other -o out.o
gcc out.o -o a.out
./a.out
