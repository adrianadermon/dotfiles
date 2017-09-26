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
