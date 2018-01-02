" Indent and tab settings
set shiftwidth=2
set softtabstop=2

" Use unix file endings
if &modifiable
    setlocal fileformat=unix
endif

" Run lines and selections with Ctrl-Enter
nmap <buffer> <C-Enter> :TREPLSendLine<CR>
vmap <buffer> <C-Enter> :TREPLSendSelection<CR>
