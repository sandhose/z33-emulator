" plugin/z33.vim — optional vim-lsp registration for the z33-cli language server.
"
" This is inert unless vim-lsp is actually installed: it only ever fires on
" vim-lsp's own `User lsp_setup` autocommand, and even then it no-ops when
" `z33-cli` isn't on PATH. Classic Vim does NOT auto-download the binary (that
" convenience lives in the Neovim plugin only) — see the README for release
" links. Opt out completely with `let g:z33_no_lsp = 1`.

if exists('g:loaded_z33')
  finish
endif
let g:loaded_z33 = 1

if get(g:, 'z33_no_lsp', 0)
  finish
endif

func! s:register_z33_lsp() abort
  " vim-lsp present?
  if !exists('*lsp#register_server')
    return
  endif
  " Binary on PATH? (No auto-download in classic Vim.)
  if !executable('z33-cli')
    return
  endif
  call lsp#register_server({
        \ 'name': 'z33-cli',
        \ 'cmd': {server_info -> ['z33-cli', 'lsp']},
        \ 'allowlist': ['z33'],
        \ })
endfunc

augroup z33_lsp
  autocmd!
  autocmd User lsp_setup call s:register_z33_lsp()
augroup END
