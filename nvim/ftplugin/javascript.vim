" Indent and tab settings
set shiftwidth=2
set softtabstop=2

" Run lines and selections with Ctrl-Enter
nmap <buffer> <C-Enter> :TREPLSendLine<CR>
vmap <buffer> <C-Enter> :TREPLSendSelection<CR>
