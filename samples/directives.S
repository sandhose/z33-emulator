.string "hello"
.word 3 + 8 * pmain
pmain: .word main
.space 5

.addr 100
main: reset
