.section .note.GNU-stack,"",@progbits
.global main
.data
_N1: .asciz "a"
_N2: .asciz "b"
_N3: .asciz "c"
_N5: .asciz "d"
_N6: .asciz "ending"
.text
main:
    push %rbp
    mov %rsp,%rbp
    push %rdi
    push %rsi
    mov -8(%rbp),%rax
    cmp $1,%rax
    je .L0_1
    cmp $2,%rax
    je .L0_2
    cmp $3,%rax
    je .L0_3
    cmp $4,%rax
    je .L0_4
    jmp .L0
.L0_1:
    lea puts(%rip),%rax
    push %rax
    lea _N1(%rip),%rax
    mov %rax,%rdi
    xor %rax,%rax
    pop %r11
    sub $8,%rsp
    call *%r11
    add $8,%rsp
    jmp end
.L0_2:
    lea puts(%rip),%rax
    push %rax
    lea _N2(%rip),%rax
    mov %rax,%rdi
    xor %rax,%rax
    pop %r11
    sub $8,%rsp
    call *%r11
    add $8,%rsp
    jmp end
.L0_3:
    lea puts(%rip),%rax
    push %rax
    lea _N3(%rip),%rax
    mov %rax,%rdi
    xor %rax,%rax
    pop %r11
    sub $8,%rsp
    call *%r11
    add $8,%rsp
    mov $2,%rax
    push %rax
    mov $1,%rax
    pop %r10
    cmp %r10,%rax
    setz %al
    movzx %al,%rax
    cmp $0, %rax
    je .L4
.L0_4:
    lea puts(%rip),%rax
    push %rax
    lea _N5(%rip),%rax
    mov %rax,%rdi
    xor %rax,%rax
    pop %r11
    sub $8,%rsp
    call *%r11
    add $8,%rsp
    add $0,%rsp
    add $0,%rsp
.L4:
    add $0,%rsp
    add $0,%rsp
.L0:
    lea puts(%rip),%rax
    push %rax
    lea _N6(%rip),%rax
    mov %rax,%rdi
    xor %rax,%rax
    pop %r11
    sub $8,%rsp
    call *%r11
    add $8,%rsp
end:
    add $0,%rsp
    xor %rax,%rax
    mov %rbp,%rsp
    pop %rbp
    ret
