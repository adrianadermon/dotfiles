" Indent and tab settings
set shiftwidth=2
set softtabstop=2

" Start R with a script that changes the codepage so that
" accented characters are interpreted correctly
let R_app = "chcpR"

" Disable assignment mapping
let R_assign = 0

" Use older Rtools for Nvim-R plugin
let Rtools_path = "C:\\Rtools3.3"

" Use color scheme for R output
let rout_follow_colorscheme = 1

" Enable folding
let r_syntax_folding = 1

" Start R in working directory
let R_nvim_wd = 1

" Close R when quitting Vim
autocmd VimLeave * if exists("g:SendCmdToR") && string(g:SendCmdToR) != "function('SendCmdToR_fake')" | call RQuit("nosave") | endif

" Insert spaces around operator only if needed
function! RSmartSpace(string)
    " Get previous character
    let prevchar = matchstr(getline("."), '\%' . (col(".")-1) . "c.")
    " Get next character
    let nextchar = matchstr(getline("."), '\%' . col(".") . "c.")

    " Check if previous character is a space
    if(prevchar == " ")
        let prefix = ""
    else
        let prefix = " "
    endif

    " Check if next character is a space
    if(nextchar == " ")
        let suffix = ""
    else
        let suffix = " "
    endif

    " Add spaces where needed
    return prefix . a:string . suffix
endfunc

" Map assignment operator
inoremap <buffer><expr> <M--> RSmartSpace("<-")

" Map magrittr pipes to corresponding keys
inoremap <buffer><expr> <M-.> RSmartSpace("%>%")
inoremap <buffer><expr> <M-,> RSmartSpace("%<>%")
inoremap <buffer><expr> <M-4> RSmartSpace("%$%")
inoremap <buffer><expr> <M-t> RSmartSpace("%T>%")

" Run lines and selections with Ctrl-Enter
nmap <C-Enter> <Plug>RDSendLine
vmap <C-Enter> <Plug>RDSendSelection

