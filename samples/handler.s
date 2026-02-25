#define EXCEPTION [102]
#define TRAP 4

main:
    trap
    reset

invalid:
    .word 0

.addr 200
handler:
    push %a
    ld EXCEPTION, %a
    cmp TRAP, %a
    jne exit
    pop %a
    rti

exit:
    reset
