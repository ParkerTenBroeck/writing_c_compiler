
            .global main
main:
	pushq %rbp
	movq %rsp, %rbp

	subq $0, %rsp
	movl $3, -20(%rbp)
	movl -20(%rbp), %r11d
	imull $2, %r11d
	movl %r11d, -20(%rbp)
	movl $5, -16(%rbp)
	movl -20(%rbp), %r10d
	addl %r10d, -16(%rbp)
	movl -16(%rbp), %r10d
	movl %r10d, -12(%rbp)
	movl -12(%rbp), %r10d
	movl %r10d, -8(%rbp)
	negl -8(%rbp)
	movl -8(%rbp), %r10d
	movl %r10d, -4(%rbp)
	addl $40, -4(%rbp)
	movl -4(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax
	movq %rbp, %rsp
	popq %rbp
	ret

.section .note.GNU-stack,"",@progbits
