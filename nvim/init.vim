set termguicolors

if has('win32')
    let g:python_host_prog = $HOME . '/Anaconda3/envs/py27/python.exe'
    let g:python3_host_prog = $HOME . '/Anaconda3/python.exe'
endif

" vim-plug
call plug#begin('~/AppData/Local/nvim/plugged')

" fzf in linux
if has('unix')
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
endif

" Base16 color themes
Plug 'chriskempson/base16-vim'

Plug 'andreypopp/vim-colors-plain'

" Start screen
Plug 'mhinz/vim-startify'

" Statusline
" Plug 'itchyny/lightline.vim'

" Base16 themes for statusline
" Plug 'felixjung/vim-base16-lightline'

" Change directory
Plug 'airblade/vim-rooter'

" Move around easily
Plug 'justinmk/vim-sneak'

" Switch to text objects
Plug 'tommcdo/vim-exchange'

" Rainbow parentheses
Plug 'junegunn/rainbow_parentheses.vim'

" Additional mappings
Plug 'tpope/vim-unimpaired'

" Additional targets
Plug 'wellle/targets.vim'

" Autocomplete
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-path'

" Javascript Tern autocompletion source
Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}

" Fuzzy finder
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Linter
Plug 'w0rp/ale'

" Asynchronous make
Plug 'tpope/vim-dispatch'

" Format lines
Plug 'sbdchd/neoformat'

" Tags
Plug 'ludovicchabant/vim-gutentags'

" Directory viewer
Plug 'justinmk/vim-dirvish'

" Work with parentheses, quotes, etc
Plug 'tpope/vim-surround'

" Make plugin maps repeatable
Plug 'tpope/vim-repeat'

" Align
Plug 'junegunn/vim-easy-align'

" Create tables
Plug 'dhruvasagar/vim-table-mode'

" Comment
Plug 'tpope/vim-commentary'

" Wrap and unwrap argument lists
Plug 'FooSoft/vim-argwrap'

" Highlight yank
Plug 'machakann/vim-highlightedyank'

" Preview color codes
Plug 'ap/vim-css-color'

" Faster folding
Plug 'Konfekt/FastFold'

" Git integration
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'

" Show git diff in gutter
" Plug 'airblade/vim-gitgutter'
Plug 'mhinz/vim-signify'

" Distraction-free writing
Plug 'junegunn/goyo.vim'

" Communicate with R
Plug 'jalvesaq/Nvim-R'

" R autocompletion
Plug 'gaalcaras/ncm-R'

" CSV files
Plug 'chrisbra/csv.vim'

" Markdown and padoc support
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

" Latex editing
Plug 'lervag/vimtex'

" Julia support
Plug 'JuliaEditorSupport/julia-vim'

" Javascript syntax
Plug 'pangloss/vim-javascript'

" REPL
Plug 'jalvesaq/vimcmdline'

" Lilypond support
Plug 'gisraptor/vim-lilypond-integrator'

" Graphviz support
Plug 'wannesm/wmgraphviz.vim'

" Haskell
Plug 'neovimhaskell/haskell-vim'

call plug#end()

" Indent and tab settings
set expandtab
set shiftwidth=4
set softtabstop=4

" Hybrid line numbers
set number
set relativenumber

" Smart search case
set ignorecase
set smartcase

" Better window splitting
set splitright
set splitbelow

" Preview substitions
set inccommand=split

" Don't split words when soft wrapping
set linebreak

" Start with all folds open
set nofoldenable

" Set fold character to blank
set fillchars=vert:│,fold:\

" Enable syntax highlighting of fenced code blocks in markdown
let g:markdown_fenced_languages= ['r', 'python', 'julia', 'stata']

" Move by display line
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
vnoremap <expr> j v:count ? 'j' : 'gj'
vnoremap <expr> k v:count ? 'k' : 'gk'

" Search with ripgrep
set grepprg=rg\ --vimgrep
set grepformat^=%f:%l:%c:%m

" Clear search highlight
nnoremap <C-l> :nohlsearch<CR><C-l>

" Space as leader
map <space> <leader>

" Set theme
" colorscheme base16-oceanicnext
set background=light
colorscheme plain
" Fix the background - should not be needed
highlight Normal guibg=#F1F1F1

" Set lightline theme
" let g:lightline = {}
" let g:lightline.colorscheme = 'base16_oceanicnext'

" Configure statusline
if has('statusline')
    function! ALEWarnings() abort
        let l:counts = ale#statusline#Count(bufnr(''))
        let l:all_errors = l:counts.error + l:counts.style_error
        let l:all_non_errors = l:counts.total - l:all_errors
        return l:counts.total == 0 ? '' : printf('  %dW ', all_non_errors)
    endfunction

    function! ALEErrors() abort
        let l:counts = ale#statusline#Count(bufnr(''))
        let l:all_errors = l:counts.error + l:counts.style_error
        let l:all_non_errors = l:counts.total - l:all_errors
        return l:counts.total == 0 ? '' : printf(' %dE ', all_errors)
    endfunction

    function! ALEStatus() abort
        let l:counts = ale#statusline#Count(bufnr(''))
        let l:all_errors = l:counts.error + l:counts.style_error
        let l:all_non_errors = l:counts.total - l:all_errors
        return l:counts.total == 0 ? ' ok ' : ''
    endfunction

    set laststatus=2
    set statusline=%<%f
    set statusline+=%w%h%m%r


    set statusline+=\ %y

    set statusline+=%=%-12.(%l,%c%V\ %p%%%)\ 

    set statusline+=%-12.(%{(&fenc!=''?&fenc:&enc)}[%{&ff}]%)

    set statusline+=%#StatusLineOk#%{ALEStatus()}
    set statusline+=%#StatusLineError#%{ALEErrors()}
    set statusline+=%#StatusLineWarning#%{ALEWarnings()}

endif


" Start screen header
let g:startify_custom_header = [
    \ '__/\\\________/\\\__/\\\\\\\\\\\__/\\\\____________/\\\\_        ',
    \ ' _\/\\\_______\/\\\_\/////\\\///__\/\\\\\\________/\\\\\\_       ',
    \ '  _\//\\\______/\\\______\/\\\_____\/\\\//\\\____/\\\//\\\_      ',
    \ '   __\//\\\____/\\\_______\/\\\_____\/\\\\///\\\/\\\/_\/\\\_     ',
    \ '    ___\//\\\__/\\\________\/\\\_____\/\\\__\///\\\/___\/\\\_    ',
    \ '     ____\//\\\/\\\_________\/\\\_____\/\\\____\///_____\/\\\_   ',
    \ '      _____\//\\\\\__________\/\\\_____\/\\\_____________\/\\\_  ',
    \ '       ______\//\\\________/\\\\\\\\\\\_\/\\\_____________\/\\\_ ',
    \ '        _______\///________\///////////__\///______________\///__',
    \ ]

" Hide help files from start screen
let g:startify_skiplist = [
    \ 'AppData\\Local\\nvim\\plugged\\.*\\doc\\.*.txt',
    \ 'Neovim\\share\\nvim\\runtime\\doc\\.*.txt',
    \ ]

" Start screen bookmarks
let g:startify_bookmarks = [ 
    \ '~/dotfiles/nvim',
    \ '~/dotfiles/nvim/init.vim',
    \ ]

" Vim-rooter settings
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_manual_only = 1

" Set private snippets directory
let g:UltiSnipsSnippetsDir = "~/AppData/Local/nvim/UltiSnips"
let g:UltiSnipsEditSplit = 'horizontal'

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Set yank highlight duration
let g:highlightedyank_highlight_duration = 500


" Autocompletion setup
""""""""""""""""""""""

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

set completeopt=noinsert,menuone,noselect

" REPL setup
""""""""""""

" vimcmdline mappings
let cmdline_map_start          = '<LocalLeader>s'
let cmdline_map_send           = '<LocalLeader>l'
let cmdline_map_send_and_stay  = '<LocalLeader>k'
let cmdline_map_source_fun     = '<LocalLeader>f'
let cmdline_map_send_paragraph = '<LocalLeader>p'
let cmdline_map_send_block     = '<LocalLeader>b'
let cmdline_map_quit           = '<LocalLeader>q'

" vimcmdline options
let cmdline_esc_term    = 1      " Remap <Esc> to :stopinsert in Neovim's terminal
let cmdline_in_buffer   = 1      " Start the interpreter in a Neovim's terminal
let cmdline_term_height = 25     " Initial height of interpreter window or pane
let cmdline_tmp_dir     = '/tmp' " Temporary directory to save files
let cmdline_outhl       = 1      " Syntax highlight the output
let cmdline_auto_scroll = 1      " Keep the cursor at the end of terminal (nvim)

let cmdline_follow_colorscheme = 1

let cmdline_app           = {}
let cmdline_app['haskell'] = 'ghci'

" Linter setup
""""""""""""""

" Map movement through errors without wrapping.
nmap <silent> <C-k> <Plug>(ale_previous)
nmap <silent> <C-j> <Plug>(ale_next)

let g:ale_fixers = {
\   'javascript': [
\       'prettier',
\       'trim_whitespace',
\       'remove_trailing_lines',
\   ],
\   'css': [
\       'prettier',
\       'trim_whitespace',
\       'remove_trailing_lines',
\   ],
\   'html': [
\       'trim_whitespace',
\       'remove_trailing_lines',
\   ],
\   'python': [
\       'yapf',
\       'trim_whitespace',
\       'remove_trailing_lines',
\   ],
\}

" Markdown setup
""""""""""""""""

let g:pandoc#spell#default_langs = ['en_us', 'sv_se']

" Fzf setup
"""""""""""""""""""""""""""""""""""""""""""""""""

" Search
nnoremap <leader>b :<C-u>Buffer<CR>
nnoremap <leader>f :<C-u>Files<CR>

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" File search with ripgrep
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

nnoremap <leader>s :<C-u>Rg<CR>

" Latex
"""""""""""""""""""""""""""""""""""""""""""""""""

" Enable vimtex autocomplete with NCM
augroup my_cm_setup
    autocmd!
    autocmd BufEnter * call ncm2#enable_for_buffer()
    autocmd Filetype tex call ncm2#register_source({
                \ 'name' : 'vimtex-cmds',
                \ 'priority': 8, 
                \ 'complete_length': -1,
                \ 'scope': ['tex'],
                \ 'matcher': {'name': 'prefix', 'key': 'word'},
                \ 'word_pattern': '\w+',
                \ 'complete_pattern': g:vimtex#re#ncm2#cmds,
                \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
                \ })
    autocmd Filetype tex call ncm2#register_source({
                \ 'name' : 'vimtex-labels',
                \ 'priority': 8, 
                \ 'complete_length': -1,
                \ 'scope': ['tex'],
                \ 'matcher': {'name': 'combine',
                \             'matchers': [
                \               {'name': 'substr', 'key': 'word'},
                \               {'name': 'substr', 'key': 'menu'},
                \             ]},
                \ 'word_pattern': '\w+',
                \ 'complete_pattern': g:vimtex#re#ncm2#labels,
                \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
                \ })
    autocmd Filetype tex call ncm2#register_source({
                \ 'name' : 'vimtex-files',
                \ 'priority': 8, 
                \ 'complete_length': -1,
                \ 'scope': ['tex'],
                \ 'matcher': {'name': 'combine',
                \             'matchers': [
                \               {'name': 'abbrfuzzy', 'key': 'word'},
                \               {'name': 'abbrfuzzy', 'key': 'abbr'},
                \             ]},
                \ 'word_pattern': '\w+',
                \ 'complete_pattern': g:vimtex#re#ncm2#files,
                \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
                \ })
    autocmd Filetype tex call ncm2#register_source({
                \ 'name' : 'bibtex',
                \ 'priority': 8, 
                \ 'complete_length': -1,
                \ 'scope': ['tex'],
                \ 'matcher': {'name': 'combine',
                \             'matchers': [
                \               {'name': 'prefix', 'key': 'word'},
                \               {'name': 'abbrfuzzy', 'key': 'abbr'},
                \               {'name': 'abbrfuzzy', 'key': 'menu'},
                \             ]},
                \ 'word_pattern': '\w+',
                \ 'complete_pattern': g:vimtex#re#ncm2#bibtex,
                \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
                \ })
augroup END

" Latexmk options
let g:vimtex_compiler_latexmk = {
    \ 'continuous' : 0,
    \}

" Folding
let g:vimtex_fold_enabled = 1

" Use SumatraPDF as viewer with vimtex
let g:vimtex_view_general_viewer = 'SumatraPDF'
let g:vimtex_view_general_options
	\ = '-reuse-instance -forward-search @tex @line @pdf'
let g:vimtex_view_general_options_latexmk = '-reuse-instance'


""""""""""""""""
