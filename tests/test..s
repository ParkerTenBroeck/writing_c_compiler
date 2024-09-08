
            .global main
main:
	pushq %rbp
	movq %rsp, %rbp

	subq $88, %rsp
	movl $2, -8(%rbp)
	negl -8(%rbp)
	movl -8(%rbp), %r10d
	movl %r10d, -12(%rbp)
	negl -12(%rbp)
	movl -12(%rbp), %r10d
	movl %r10d, -16(%rbp)
	negl -16(%rbp)
	movl -16(%rbp), %r10d
	movl %r10d, -20(%rbp)
	negl -20(%rbp)
	movl -20(%rbp), %r10d
	movl %r10d, -24(%rbp)
	negl -24(%rbp)
	movl -24(%rbp), %r10d
	movl %r10d, -28(%rbp)
	negl -28(%rbp)
	movl -28(%rbp), %r10d
	movl %r10d, -32(%rbp)
	negl -32(%rbp)
	movl -32(%rbp), %r10d
	movl %r10d, -36(%rbp)
	negl -36(%rbp)
	movl -36(%rbp), %r10d
	movl %r10d, -40(%rbp)
	negl -40(%rbp)
	movl -40(%rbp), %r10d
	movl %r10d, -44(%rbp)
	negl -44(%rbp)
	movl -44(%rbp), %r10d
	movl %r10d, -48(%rbp)
	negl -48(%rbp)
	movl -48(%rbp), %r10d
	movl %r10d, -52(%rbp)
	negl -52(%rbp)
	movl -52(%rbp), %r10d
	movl %r10d, -56(%rbp)
	negl -56(%rbp)
	movl -56(%rbp), %r10d
	movl %r10d, -60(%rbp)
	negl -60(%rbp)
	movl -60(%rbp), %r10d
	movl %r10d, -64(%rbp)
	negl -64(%rbp)
	movl -64(%rbp), %r10d
	movl %r10d, -68(%rbp)
	negl -68(%rbp)
	movl -68(%rbp), %r10d
	movl %r10d, -72(%rbp)
	negl -72(%rbp)
	movl -72(%rbp), %r10d
	movl %r10d, -76(%rbp)
	negl -76(%rbp)
	movl -76(%rbp), %r10d
	movl %r10d, -80(%rbp)
	negl -80(%rbp)
	movl -80(%rbp), %r10d
	movl %r10d, -84(%rbp)
	negl -84(%rbp)
	movl -84(%rbp), %r10d
	movl %r10d, -88(%rbp)
	negl -88(%rbp)
	movl -88(%rbp), %r10d
	movl %r10d, -4(%rbp)
	movl -4(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax
	movq %rbp, %rsp
	popq %rbp
	ret

.section .note.GNU-stack,"",@progbits
