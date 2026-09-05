" ftdetect/z33.vim — filetype detection for Zorglub-33 assembly.
"
" The hard part: Z33 sources use the .s/.S extension, which Vim's builtin
" filetype.vim already claims for GNU/other `asm`. We must only steal those
" buffers when the content is unmistakably Z33, so real GNU asm keeps working.
" The same extension is also claimed by vim-polyglot's r-lang detector
" (legacy S-PLUS used `.s`); when polyglot's autocmd runs first, the buffer
" arrives here as `r` instead of `asm`, so the guard below allows overriding
" that too.
"
" Controls (all documented in the README):
"   g:z33_no_ftdetect  — if set, skip this heuristic entirely.
"   g:z33_filetypes    — if set, force *every* .s/.S buffer to z33.
"   modeline           — `// vim: ft=z33` wins on its own (Vim applies
"                        modelines after ftdetect); nothing to implement.

" Neovim has its own detection path (lua/z33/ftdetect.lua, armed by the Lua
" `ftdetect/z33.lua` + `vim.filetype.add`). Bail out here so the two don't
" install duplicate `.s`/`.S` autocmds under Neovim.
if has('nvim')
  finish
endif

if get(g:, 'z33_no_ftdetect', 0)
  finish
endif

" Content heuristic, kept in lockstep with the Lua copy in
" lua/z33/ftdetect.lua (same window, same signal lists). Two of the four
" patterns are matched case-sensitively (=~#) below.
"
" Anti-signals: GNU spellings Z33 does not have. Z33 has `#if` and `#undefine`
" but no `#ifdef`/`#ifndef`/`#undef`/`#pragma`, its `#include` takes a quoted
" string only, and its four directives are `.addr`/`.space`/`.string`/`.word`.
let s:anti =
      \   '^\s*#\s*\%(ifdef\|ifndef\|undef\|pragma\)\>'
      \ . '\|^\s*#\s*include\s*<'
      \ . '\|^\s*\.\%(globl\|global\|section\|type\|size\|text\|data\|macro\|endm\)\>'

" Plan 9 / Go assembly, which spells these in upper case. Go's `.s` files are
" otherwise mistaken for Z33: `#include "textflag.h"` is a positive below and
" JEQ/JLT/JGT are Plan 9 mnemonics too. `(SB)` is the dialect's static-base
" pseudo-register, on every symbol reference.
let s:plan9 = '^\%(TEXT\|DATA\|GLOBL\|FUNCDATA\|PCDATA\)\%([ \t]\|·\)\|(SB)'

" Positives, part one: the preprocessor. `#` in column 0 and a lower-case
" keyword is the only spelling parser/preprocessor.rs accepts. These keywords
" are shared with C, hence only trusted because an anti-signal anywhere in the
" window vetoes the file.
let s:preproc =
      \   '^#\s*\%(undefine\|define\|if\|elif\|endif\|error\)\>'
      \ . '\|^#\s*include\s*"'

" Positives, part two: directives, registers and mnemonics, all
" case-insensitive to the parser. The %pc/%sr/%a/%b registers (GNU asm uses
" %eax, %rdi, … never %pc; %sp is excluded, it is a real x86 AT&T register,
" and the word boundary keeps %a off m68k's %a0-%a7), the .addr directive, and
" mnemonics rare in GNU asm — push/pop/call/jmp/add/… are excluded, and of the
" conditional jumps only the jeq/jlt/jgt spellings (x86 has jle/jge/jne).
" m68k does spell reset/rti/swap/trap this way, but a real m68k file also
" carries a .text/.globl/.section veto.
let s:positive =
      \   '^\s*\.addr\>'
      \ . '\|%\%(pc\|sr\|a\|b\)\>'
      \ . '\|^\s*\%(\w\+\s*:\s*\)\=\%(fas\|rti\|rtn\|swap\|reset\|trap\|jeq\|jlt\|jgt\)\>'

" `setlocal filetype=z33` rather than `:setf z33`: this autocommand runs after
" the builtin `.s`/`.S` detection has set `asm`, and `:setf` does nothing once
" a filetype has been set in the current autocommand sequence. (A trailing
" `// vim: ft=…` modeline still wins — modelines are applied after ftdetect.)
func! s:DetectZ33() abort
  " Never clobber a filetype another (more confident) detector already set;
  " only act on a fresh buffer, one the builtin fell back to `asm`, or one
  " vim-polyglot's r-lang detector claimed as `r` (see header comment).
  if &filetype !=# '' && &filetype !=# 'asm' && &filetype !=# 'r'
    return
  endif

  " Opt-in: user only ever writes Z33, force it unconditionally.
  if get(g:, 'z33_filetypes', 0)
    setlocal filetype=z33
    return
  endif

  let l:found = 0
  for l:line in getline(1, 64)
    if l:line =~? s:anti || l:line =~# s:plan9
      return
    endif
    if l:line =~# s:preproc || l:line =~? s:positive
      let l:found = 1
    endif
  endfor

  if l:found
    setlocal filetype=z33
  endif
endfunc

augroup z33_ftdetect
  autocmd!
  autocmd BufRead,BufNewFile *.s,*.S call s:DetectZ33()
augroup END
