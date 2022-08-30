nasm -f elf64 orth/output.asm -o orth/output.o
ld orth/output.o -o orth/output
./orth/output one two three
echo $?