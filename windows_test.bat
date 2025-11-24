@echo off

"C:\Program Files\NASM\nasm.exe" -fwin64 output.asm -o output.obj
if %errorlevel% neq 0 (
    echo nasm error
    exit /b 1
)

lld-link /machine:x64 /subsystem:console /out:output.exe output.obj kernel32.lib ^
    legacy_stdio_definitions.lib msvcrt.lib
if %errorlevel% neq 0 (
    echo lld-link error
    exit /b 1
)