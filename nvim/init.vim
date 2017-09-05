set termguicolors

let g:python_host_prog = 'C:/Users/adria/Anaconda3/envs/py27/python.exe'
let g:python3_host_prog = 'C:/Users/adria/Anaconda3/python.exe'

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

call plug#end()

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

" Use SumatraPDF as viewer with vimtex
let g:vimtex_view_general_viewer = 'SumatraPDF'
"let g:vimtex_view_general_options
"	\ = '-reuse-instance -forward-search @tex @line @pdf'
let g:vimtex_view_general_options
	\ = '@pdf'
let g:vimtex_view_general_options_latexmk = '-reuse-instance'


""""""""""""""""
