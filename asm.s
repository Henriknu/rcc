	.comm	jim,8,8
	.comm	fred,8,8
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

	movq	$5, %r8
	movq	%r8, fred(%rip)
	movq	$12, %r8
	movq	%r8, jim(%rip)
	movq	fred(%rip), %r8
	movq	jim(%rip), %r9
	addq	%r8, %r9
	movq	%r9, %rdi
	call	printint
	movq	fred(%rip), %r8
	movq	fred(%rip), %r10
	subq	%r10, %r8
	movq	jim(%rip), %r10
	movq	$2, %r11
	imulq	%r10, %r11
	addq	%r8, %r11
	movq	%r11, %rdi
	call	printint

	movl	$0, %eax
	popq	%rbp
	ret
