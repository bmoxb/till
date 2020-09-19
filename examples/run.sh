cd ..
cargo run examples/$1 examples/out.asm
cd examples/
nasm -f elf64 out.asm -F Dwarf
ld out.o -plugin /usr/libexec/gcc/x86_64-redhat-linux/10/liblto_plugin.so -plugin-opt=/usr/libexec/gcc/x86_64-redhat-linux/10/lto-wrapper -plugin-opt=-fresolution=/tmp/ccRuhiRq.res -plugin-opt=-pass-through=-lgcc -plugin-opt=-pass-through=-lgcc_s -plugin-opt=-pass-through=-lc -plugin-opt=-pass-through=-lgcc -plugin-opt=-pass-through=-lgcc_s --build-id --no-add-needed --eh-frame-hdr --hash-style=gnu -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/gcc/x86_64-redhat-linux/10/../../../../lib64/crt1.o /usr/lib/gcc/x86_64-redhat-linux/10/../../../../lib64/crti.o /usr/lib/gcc/x86_64-redhat-linux/10/crtbegin.o -L/usr/lib/gcc/x86_64-redhat-linux/10 -L/usr/lib/gcc/x86_64-redhat-linux/10/../../../../lib64 -L/lib/../lib64 -L/usr/lib/../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/10/../../.. -lgcc --push-state --as-needed -lgcc_s --pop-state -lc -lgcc --push-state --as-needed -lgcc_s --pop-state /usr/lib/gcc/x86_64-redhat-linux/10/crtend.o /usr/lib/gcc/x86_64-redhat-linux/10/../../../../lib64/crtn.o
./a.out
