#!/bin/bash

nasm -g -f elf64 -o output.o output.asm && gcc output.o -o output && ./output
echo $?

# todo - objdump
