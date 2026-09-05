	.text
	.globl _start
_start:
	move.l %a0,%d0
	swap %d0
	reset
