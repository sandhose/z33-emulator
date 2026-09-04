#include <asm/unistd.h>
#define SYS_write 1
	movq $SYS_write, %rax
	syscall
