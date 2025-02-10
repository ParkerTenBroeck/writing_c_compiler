
            .global main
main:
	pushq %rbp
	movq %rsp, %rbp

	subq $0, %rsp
	movl $0, -8(%rbp)
.L1:
	movl $50, %r10d
	cmpl -8(%rbp), %r10d
	movl $0, -4(%rbp)
	movl $50, %r10d
	cmpl -8(%rbp), %r10d
	setg -4(%rbp)
	movl $0, %r10d
	cmpl -4(%rbp), %r10d
	je .L0
	movl -8(%rbp), %r10d
	movl %r10d, -8(%rbp)
	addl $1, -8(%rbp)
	jmp .L1
.L0:
	movl -8(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax
	movq %rbp, %rsp
	popq %rbp
	ret

.section .note.GNU-stack,"",@progbits
