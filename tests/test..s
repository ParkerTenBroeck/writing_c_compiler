.global main
main:
	pushq %rbp
	movq %rsp, %rbp

	subq $4, %rsp
	movl $0, %r10d
	cmpl $2, %r10d
	movl $0, -4(%rbp)
	movl $0, %r10d
	cmpl $2, %r10d
	sete -4(%rbp)
	movl -4(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret

.section .note.GNU-stack,"",@progbits
