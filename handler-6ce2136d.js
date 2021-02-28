export default"#define EXCEPTION [102]\n#define TRAP 4\n\nmain:\n    trap\n    reset\n\ninvalid:\n    .word 0\n\n.addr 200\nhandler:\n    push %a\n    ld EXCEPTION, %a\n    cmp TRAP, %a\n    jne exit\n    pop %a\n    rti\n\nexit:\n    reset\n";
//# sourceMappingURL=handler-6ce2136d.js.map
