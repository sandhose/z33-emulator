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

" We use `setlocal filetype=z33` rather than `:setf z33` on purpose: our
" autocommand runs *after* the builtin `.s`/`.S` detection has already set
" `asm`, and `:setf` is defined to do nothing once a filetype has been set in
" the current autocommand sequence — it could never override `asm`. A plain
" `setlocal filetype=` forces it. (A trailing `// vim: ft=…` modeline still
" wins, since modelines are applied after ftdetect.)
func! s:DetectZ33() abort
  " Never clobber a filetype another (more confident) detector already set;
  " only act on a fresh buffer, one the builtin fell back to `asm`, or one
  " vim-polyglot's r-lang detector claimed as `r` (see header comment above
  " — that's the S-PLUS `.s` collision, not a hypothetical). The content
  " heuristic below only fires on unmistakably-Z33 markers, so it's safe to
  " override an `r` guess the same way we override `asm`.
  if &filetype !=# '' && &filetype !=# 'asm' && &filetype !=# 'r'
    return
  endif

  " Opt-in: user only ever writes Z33, force it unconditionally.
  if get(g:, 'z33_filetypes', 0)
    setlocal filetype=z33
    return
  endif

  " Content heuristic. Scan the head of the buffer for signals that are
  " specific to Z33 and effectively never appear in GNU/other asm:
  "   - the %pc/%sr/%a/%b registers (GNU asm uses %eax, %rdi, … never %pc;
  "     %sp was dropped — it's a real x86 AT&T register, so it caused false
  "     positives on GNU asm. Bare %a/%b are safe: GNU asm has no such
  "     registers, and the `\>` word-boundary keeps them from matching inside
  "     %ax/%bp/etc.);
  "   - the Z33 .addr directive (.word/.space/.string were dropped — they're
  "     standard GNU `as` directives too, not Z33-specific);
  "   - the Z33 preprocessor (#include/#define/#undefine/#if/#elif/#endif/#error
  "     — note `#undefine`, not the C-style `#undef`, and `#` may be spaced).
  " A bare `//` comment is deliberately NOT a signal (GNU asm allows it too).
  let l:lines = getline(1, 64)
  for l:line in l:lines
    if l:line =~? '^\s*#\s*\%(include\|define\|undefine\|if\|elif\|endif\|error\)\>'
          \ || l:line =~? '^\s*\.addr\>'
          \ || l:line =~? '%\%(pc\|sr\|a\|b\)\>'
      setlocal filetype=z33
      return
    endif
  endfor
endfunc

augroup z33_ftdetect
  autocmd!
  autocmd BufRead,BufNewFile *.s,*.S call s:DetectZ33()
augroup END
