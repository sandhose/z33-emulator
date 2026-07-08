" syntax/z33.vim — syntax highlighting for Zorglub-33 assembly.
" Language:   Zorglub-33 (z33) assembly
" Maintainer: z33-emulator (https://github.com/sandhose/z33-emulator)

if exists('b:current_syntax')
  finish
endif

" Mnemonics, registers and directive names are case-insensitive in Z33
" (the reference parser lowercases them). Preprocessor keywords are NOT — they
" are matched case-sensitively below with an explicit \C.
syntax case ignore

" --- Strings (defined BEFORE comments) --------------------------------------
" A `//` sequence inside a string must stay part of the string, so the string
" region has to exist before the comment match can claim the `//`.
" Escapes: \\  \"  \n  plus a backslash-newline line continuation; skip=/\\./
" keeps an escaped quote from prematurely ending the region.
syntax match  z33Escape /\\[\\"n]/ contained
syntax match  z33Escape /\\$/ contained
syntax region z33String start=/"/ skip=/\\./ end=/"/ contains=z33Escape

" --- Instruction mnemonics (case-insensitive, 33 of them) -------------------
syntax keyword z33Mnemonic add and call cmp div fas in jmp jeq jne jle jlt jge
syntax keyword z33Mnemonic jgt ld mul neg nop not or out pop push reset rti rtn
syntax keyword z33Mnemonic shl shr st sub swap trap xor

" --- Registers: %a %b %pc %sp %sr (case-insensitive) ------------------------
syntax match z33Register /%\%(pc\|sp\|sr\|a\|b\)\>/

" --- Assembler directives: .word .space .string .addr ----------------------
syntax match z33Directive /\.\%(word\|space\|string\|addr\)\>/

" --- Preprocessor (case-sensitive lowercase; `#` may be spaced) -------------
" `\C` forces case-sensitivity despite the buffer-wide `syntax case ignore`.
" Note: `#undefine`, not the C-style `#undef`.
syntax match z33PreProc /\C^\s*#\s*\%(include\|define\|undefine\|error\|if\|elif\|else\|endif\)\>/

" --- Labels: an identifier immediately followed by `:` ----------------------
syntax match z33Label /\<\h\w*\ze\s*:/

" --- Number literals (unsigned; order specific bases before decimal) --------
syntax match z33Number /\<0[xX]\x\+\>/
syntax match z33Number /\<0[oO]\o\+\>/
syntax match z33Number /\<0[bB][01]\+\>/
syntax match z33Number /\<\d\+\>/

" --- Operators & punctuation ------------------------------------------------
" Expression operators: | & << >> + - * / and unary ~ / -.
syntax match z33Operator /<<\|>>\|[-+*/|&~]/
" Operand separator, label terminator and addressing brackets.
syntax match z33Delimiter /[,:[\]()]/

" --- Comments (defined LAST) ------------------------------------------------
" Must out-prioritize the single-`/` operator match so `//` always reads as a
" comment; a `//` inside a string stays part of the (earlier) string region.
syntax match z33Comment "//.*$" contains=@Spell

highlight default link z33Comment   Comment
highlight default link z33Mnemonic  Keyword
highlight default link z33Register  Identifier
highlight default link z33Directive PreProc
highlight default link z33PreProc   PreProc
highlight default link z33Label     Function
highlight default link z33Number    Number
highlight default link z33String    String
highlight default link z33Escape    SpecialChar
highlight default link z33Operator  Operator
highlight default link z33Delimiter Delimiter

let b:current_syntax = 'z33'
