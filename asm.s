	.comm	a,8,8
	.text
.LC0:
	.string	"%d\n"
printint:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	nop
	leave
	ret

	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp

	movq	$3, %r8
	movq	%r8, a(%rip)
	movq	a(%rip), %r8
	movq	$2, %r9
	cmpq	%r9, %r8
	jne	L0
	movq	$1, %r10
	movq	%r10, %rdi
	call	printint
	jmp	L1
L0:
L1:
	movq	$2, %r10
	movq	%r10, %rdi
	call	printint

	movl	$0, %eax
	popq	%rbp
	ret
