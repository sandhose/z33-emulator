val: .word 42

main:
    ld       1, %a
    swap [val], %a # swap between an address and a register
    ld   [val], %b
    debugreg       # %a should be 42 and %b should be 1
    swap    %a, %b # swap between two registers
    debugreg       # %b should be 42 and %a should be 1
    reset
