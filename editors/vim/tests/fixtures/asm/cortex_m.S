// ARM Cortex-M startup: `Reset_Handler` is not the `reset` mnemonic.
	.syntax unified
	.thumb
	.section .isr_vector,"a",%progbits
	.word _estack
	.word Reset_Handler
	.text
	.thumb_func
Reset_Handler:
	bl main
	b .
