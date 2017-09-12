set termguicolors

let g:python_host_prog = $HOME . '/Anaconda3/envs/py27/python.exe'
let g:python3_host_prog = $HOME . '/Anaconda3/python.exe'

" vim-plug
call plug#begin('~/AppData/Local/nvim/plugged')

" Base16 color themes
Plug 'chriskempson/base16-vim'

" Seoul 256 color theme
Plug 'junegunn/seoul256.vim'

" Statusline
Plug 'itchyny/lightline.vim'

" Base16 themes for statusline
Plug 'felixjung/vim-base16-lightline'

" Rainbow parentheses
Plug 'junegunn/rainbow_parentheses.vim'

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Fuzzy finder
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Linter
Plug 'w0rp/ale'

" Tags
Plug 'ludovicchabant/vim-gutentags'

" Work with parentheses, quotes, etc
Plug 'tpope/vim-surround'

" Align
Plug 'junegunn/vim-easy-align'

" Comment
Plug 'tpope/vim-commentary'

" Faster folding
Plug 'Konfekt/FastFold'

" Git integration
Plug 'tpope/vim-fugitive'

" Show git diff in gutter
Plug 'airblade/vim-gitgutter'

" Distraction-free writing
Plug 'junegunn/goyo.vim'

" Communicate with R
Plug 'jalvesaq/Nvim-R'

" CSV files
Plug 'chrisbra/csv.vim'

" Latex editing
Plug 'lervag/vimtex'

" Julia support
Plug 'JuliaEditorSupport/julia-vim'

" REPL
Plug 'kassio/neoterm'

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

" Don't split words when soft wrapping
set linebreak

" Set fold character to blank
set fillchars="vert:|,fold:\"

" Move by display line
nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
vnoremap <expr> j v:count ? 'j' : 'gj'
vnoremap <expr> k v:count ? 'k' : 'gk'

" Set theme
colorscheme base16-oceanicnext
" colorscheme seoul256

" Set lightline theme
let g:lightline = {}
let g:lightline.colorscheme = 'base16_oceanicnext'

" Use deoplete
let g:deoplete#enable_at_startup = 1

" Set private snippets directory
let g:UltiSnipsSnippetsDir = "~/AppData/Local/nvim/UltiSnips"

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" R
"""""""""""""""""""""""""""""""""""""""""""""""""

" Use older Rtools for Nvim-R plugin
let Rtools_path = "C:\\Rtools3.3"

" Use color scheme for R output
let rout_follow_colorscheme = 1

" Latex
"""""""""""""""""""""""""""""""""""""""""""""""""

" Enable vimtex autocomplete with deoplete
if !exists('g:deoplete#omni#input_patterns')
	let g:deoplete#omni#input_patterns = {}
endif
let g:deoplete#omni#input_patterns.tex = g:vimtex#re#deoplete

" Latexmk options
let g:vimtex_compiler_latexmk = {
    \ 'continuous' : 0,
    \}

" Folding
let g:vimtex_fold_enabled = 1

" Use SumatraPDF as viewer with vimtex
let g:vimtex_view_general_viewer = 'SumatraPDF'
"let g:vimtex_view_general_options
"	\ = '-reuse-instance -forward-search @tex @line @pdf'
let g:vimtex_view_general_options
	\ = '@pdf'
let g:vimtex_view_general_options_latexmk = '-reuse-instance'


""""""""""""""""
