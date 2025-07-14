section .rodata
msg db "Hello, Володя--!",10
len equ $-msg

section .text
global _start
_start:
    mov rax, 1        ; sys_write
    mov rdi, 1        ; fd = stdout
    lea rsi, [rel msg]
    mov rdx, len
    syscall

    mov rax, 60       ; sys_exit
    xor rdi, rdi
    syscall