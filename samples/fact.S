.addr 100
main:
    push 5
    call factorielle
    reset

.addr 200
factorielle:
    ld   [%sp+1],%a
    cmp  1,%a
    jge  casparticulier

    // cas général
    sub  1,%a         // a ← n-1
    push %a
    call factorielle  // a ← (n-1) !
    add  1,%sp        // dépile l’argument n-1
    push %b           // sauvegarder b
    ld   [%sp+2],%b   // b ← n (argument original)
    mul  %b,%a        // a ← n * (n-1)!
    pop  %b           // restaurer b
    rtn

.addr 300
casparticulier:
    ld  1, %a
    rtn
