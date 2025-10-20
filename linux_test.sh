#!/bin/bash

nasm -g -f elf64 -o output.o output.asm && gcc -no-pie output.o -o output && ./output
echo -e "\n$?"

# todo - objdump
