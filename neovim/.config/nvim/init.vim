set termguicolors

set guifont=PragmataPro\ Mono:h11

if has('win32')
    let g:python_host_prog = $HOME . '/Anaconda3/envs/py27/python.exe'
    let g:python3_host_prog = $HOME . '/Anaconda3/python.exe'
endif

" Packer package manager
lua require('plugins')

" Automatically re-compile package loader file when saving changes
autocmd BufWritePost plugins.lua source <afile> | PackerCompile

" Telescope setup

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>


au TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=250, on_visual=true}


" Iron REPL settings
let g:iron_map_defaults = 0
let g:iron_map_extended = 0

nmap <localleader>r <Plug>(iron-send-motion)
vmap <localleader>r <Plug>(iron-visual-send)
nmap <localleader>l <Plug>(iron-send-line)



" " OS-specific plugins
" if has('unix')
"     " fzf
"     Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
"     Plug 'junegunn/fzf.vim'
    
"     " Color theme
"     " Plug 'chriskempson/base16-vim'
"     Plug 'embark-theme/vim', { 'as': 'embark' }
"     Plug 'bluz71/vim-nightfly-guicolors'
"     Plug 'drewtempelmeyer/palenight.vim'
"     Plug 'rakr/vim-one'
" else
"     " Color theme
"     Plug 'andreypopp/vim-colors-plain'
" endif


" " Start screen
" Plug 'mhinz/vim-startify'

" " Change directory
" Plug 'airblade/vim-rooter'

" " Move around easily
" Plug 'justinmk/vim-sneak'

" " Switch to text objects
" Plug 'tommcdo/vim-exchange'

" " Rainbow parentheses
" Plug 'junegunn/rainbow_parentheses.vim'

" " Additional mappings
" Plug 'tpope/vim-unimpaired'

" " Additional targets
" Plug 'wellle/targets.vim'

" " Auto-pair parentheses etc
" Plug 'tmsvg/pear-tree'

" " Autocomplete
" Plug 'ncm2/ncm2'
" Plug 'roxma/nvim-yarp'
" Plug 'ncm2/ncm2-path'

" " Javascript Tern autocompletion source
" Plug 'ncm2/ncm2-tern',  {'do': 'npm install'}

" " Jupyter console integration
" if has('win32')
"     Plug 'jupyter-vim/jupyter-vim'
" endif

" " Python Jedi autocompletion source
" Plug 'ncm2/ncm2-jedi'

" " Fuzzy finder
" Plug 'junegunn/fzf'
" Plug 'junegunn/fzf.vim'

" " Snippets
" Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'

" " Linter
" Plug 'dense-analysis/ale'

" " Asynchronous make
" Plug 'tpope/vim-dispatch'

" " Format lines
" Plug 'sbdchd/neoformat'

" " Tags
" Plug 'ludovicchabant/vim-gutentags'

" " Directory viewer
" Plug 'justinmk/vim-dirvish'

" " Work with parentheses, quotes, etc
" Plug 'tpope/vim-surround'

" " Make plugin maps repeatable
" Plug 'tpope/vim-repeat'

" " Align
" Plug 'junegunn/vim-easy-align'

" " Create tables
" Plug 'dhruvasagar/vim-table-mode'

" " Comment
" Plug 'tpope/vim-commentary'

" " Wrap and unwrap argument lists
" Plug 'FooSoft/vim-argwrap'

" " Highlight yank
" Plug 'machakann/vim-highlightedyank'

" " Preview color codes
" Plug 'ap/vim-css-color'

" " Faster folding
" Plug 'Konfekt/FastFold'

" " Git integration
" Plug 'tpope/vim-fugitive'
" Plug 'tpope/vim-rhubarb'

" " Show git diff in gutter
" Plug 'mhinz/vim-signify'

" " Distraction-free writing
" Plug 'junegunn/goyo.vim'

" " Communicate with R
" Plug 'jalvesaq/Nvim-R'

" " R autocompletion
" Plug 'gaalcaras/ncm-R'

" " CSV files
" Plug 'chrisbra/csv.vim'

" " Markdown and padoc support
" Plug 'vim-pandoc/vim-pandoc'
" Plug 'vim-pandoc/vim-pandoc-syntax'

" " Latex editing
" Plug 'lervag/vimtex'

" " Julia support
" Plug 'JuliaEditorSupport/julia-vim'

" " Javascript syntax
" Plug 'pangloss/vim-javascript'

" " REPL
" Plug 'jalvesaq/vimcmdline'

" " Lilypond support
" Plug 'gisraptor/vim-lilypond-integrator'

" " Graphviz support
" Plug 'wannesm/wmgraphviz.vim'

" " Haskell
" Plug 'neovimhaskell/haskell-vim'

" call plug#end()

" OS-specific settings
if has('unix')
    " Color theme
    colorscheme embark
else
    " Color theme
    set background=light
    colorscheme plain
endif

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


" " Configure statusline
" if has('statusline')
"     function! ALEWarnings() abort
"         let l:counts = ale#statusline#Count(bufnr(''))
"         let l:all_errors = l:counts.error + l:counts.style_error
"         let l:all_non_errors = l:counts.total - l:all_errors
"         return l:counts.total == 0 ? '' : printf('  %dW ', all_non_errors)
"     endfunction
" 
"     function! ALEErrors() abort
"         let l:counts = ale#statusline#Count(bufnr(''))
"         let l:all_errors = l:counts.error + l:counts.style_error
"         let l:all_non_errors = l:counts.total - l:all_errors
"         return l:counts.total == 0 ? '' : printf(' %dE ', all_errors)
"     endfunction
" 
"     function! ALEStatus() abort
"         let l:counts = ale#statusline#Count(bufnr(''))
"         let l:all_errors = l:counts.error + l:counts.style_error
"         let l:all_non_errors = l:counts.total - l:all_errors
"         return l:counts.total == 0 ? ' ok ' : ''
"     endfunction
" 
"     set laststatus=2
"     set statusline=%<%f
"     set statusline+=%w%h%m%r
" 
" 
"     set statusline+=\ %y
" 
"     set statusline+=%=%-12.(%l,%c%V\ %p%%%)\ 
" 
"     set statusline+=%-12.(%{(&fenc!=''?&fenc:&enc)}[%{&ff}]%)
" 
"     set statusline+=%#StatusLineOk#%{ALEStatus()}
"     set statusline+=%#StatusLineError#%{ALEErrors()}
"     set statusline+=%#StatusLineWarning#%{ALEWarnings()}
" 
" endif


" Enable smart pairs for pear-tree
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1

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
    \ '~/dotfiles/neovim/.config/nvim',
    \ '~/dotfiles/neovim/.config/nvim/init.vim',
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


" " Autocompletion setup
" """"""""""""""""""""""
" 
" " enable ncm2 for all buffers
" autocmd BufEnter * call ncm2#enable_for_buffer()
" 
" set completeopt=noinsert,menuone,noselect

"" REPL setup
"""""""""""""

"" vimcmdline mappings
"let cmdline_map_start          = '<LocalLeader>s'
"let cmdline_map_send           = '<LocalLeader>l'
"let cmdline_map_send_and_stay  = '<LocalLeader>k'
"let cmdline_map_source_fun     = '<LocalLeader>f'
"let cmdline_map_send_paragraph = '<LocalLeader>p'
"let cmdline_map_send_block     = '<LocalLeader>b'
"let cmdline_map_quit           = '<LocalLeader>q'

"" vimcmdline options
"let cmdline_esc_term    = 1      " Remap <Esc> to :stopinsert in Neovim's terminal
"let cmdline_in_buffer   = 1      " Start the interpreter in a Neovim's terminal
"let cmdline_term_height = 25     " Initial height of interpreter window or pane
"let cmdline_tmp_dir     = '/tmp' " Temporary directory to save files
"let cmdline_outhl       = 1      " Syntax highlight the output
"let cmdline_auto_scroll = 1      " Keep the cursor at the end of terminal (nvim)

"let cmdline_follow_colorscheme = 1

"let cmdline_app           = {}
"let cmdline_app['julia'] = 'julia --color=no'
"let cmdline_app['haskell'] = 'ghci'
"let cmdline_app['python'] = 'python'

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

""""""""""""""""
