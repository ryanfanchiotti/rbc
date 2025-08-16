.global main
.data
_N2: .asciz "i = %d\n"
.text
main:
    push %rbp
    mov %rsp,%rbp
    pushq $0
    mov %rbp,%rax
    sub $8,%rax
    push %rax
    mov $0,%rax
    pop %r10
    mov %rax,(%r10)
.L0:
    mov $15,%rax
    push %rax
    mov -8(%rbp),%rax
    pop %r10
    cmp %r10,%rax
    setl %al
    movzx %al,%rax
    cmp $0, %rax
    je .L1
    lea printf(%rip),%rax
    push %rax
    lea _N2(%rip),%rax
    mov %rax,%rdi
    mov -8(%rbp),%rax
    mov %rax,%rsi
    xor %rax,%rax
    pop %r11
    call *%r11
    mov %rbp,%rax
    sub $8,%rax
    push %rax
    mov $1,%rax
    push %rax
    mov -8(%rbp),%rax
    pop %r10
    add %r10,%rax
    pop %r10
    mov %rax,(%r10)
    add $8,%rsp
    add $0,%rsp
    jmp .L0
.L1:
    add $8,%rsp
    xor %rax,%rax
    mov %rbp,%rsp
    pop %rbp
    ret
