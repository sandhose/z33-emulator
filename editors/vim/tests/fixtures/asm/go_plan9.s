#include "textflag.h"

// func add(a, b int64) int64
TEXT ·add(SB),NOSPLIT,$0-24
	MOVQ a+0(FP), AX
	MOVQ b+8(FP), BX
	ADDQ BX, AX
	JEQ done
done:
	MOVQ AX, ret+16(FP)
	RET
