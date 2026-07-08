" ftplugin/z33.vim — buffer-local defaults for Zorglub-33 assembly.

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

" Z33 line comments are `//`.
setlocal commentstring=//\ %s
setlocal comments=://

" 4-space soft tabs (matches the Zed grammar's tab_size = 4).
setlocal expandtab
setlocal shiftwidth=4
setlocal softtabstop=4

" Treat `%` as part of a keyword so `%sp` etc. behave as single words for
" w/e/b/* and are hover/complete-friendly.
setlocal iskeyword+=%

let b:undo_ftplugin = 'setlocal commentstring< comments<'
      \ . ' expandtab< shiftwidth< softtabstop< iskeyword<'
