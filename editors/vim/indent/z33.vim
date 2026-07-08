" indent/z33.vim — minimal indentation for Zorglub-33 assembly.
"
" Z33 is not block-structured: by convention labels sit flush-left and
" instructions are indented one level. There is nothing to auto-indent beyond
" (a) keeping the previous line's indent and (b) snapping label definitions
" (`name:`) back to column 0. Disable entirely with `let g:z33_no_indent = 1`.

if get(g:, 'z33_no_indent', 0)
  finish
endif

if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=Z33Indent()
setlocal indentkeys=!^F,o,O,:

let b:undo_indent = 'setlocal indentexpr< indentkeys<'

" Guard against redefinition when the file is sourced more than once.
if exists('*Z33Indent')
  finish
endif

func! Z33Indent() abort
  let l:prev = prevnonblank(v:lnum - 1)
  if l:prev == 0
    return 0
  endif
  " Label definitions go flush-left.
  if getline(v:lnum) =~# '^\s*\h\w*\s*:'
    return 0
  endif
  return indent(l:prev)
endfunc
