" Indent and tab settings
set shiftwidth=4
set softtabstop=4

" Run lines and selections with Ctrl-Enter
nmap <buffer> <C-Enter> :TREPLSendLine<CR>
vmap <buffer> <C-Enter> :TREPLSendSelection<CR>
