set termguicolors

let g:python_host_prog = $HOME . '/Anaconda3/envs/py27/python.exe'
let g:python3_host_prog = $HOME . '/Anaconda3/python.exe'

" vim-plug
call plug#begin('~/AppData/Local/nvim/plugged')

" Base16 color themes
Plug 'chriskempson/base16-vim'

" Seoul 256 color theme
Plug 'junegunn/seoul256.vim'

" Start screen
Plug 'mhinz/vim-startify'

" Statusline
Plug 'itchyny/lightline.vim'

" Base16 themes for statusline
Plug 'felixjung/vim-base16-lightline'

" Change directory
Plug 'airblade/vim-rooter'

" Rainbow parentheses
Plug 'junegunn/rainbow_parentheses.vim'

" Additional mappings
Plug 'tpope/vim-unimpaired'

" Additional targets
Plug 'wellle/targets.vim'

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Fuzzy finder
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }

" History support for finder
Plug 'Shougo/neomru.vim'

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

" Preview substitions
set inccommand=split

" Don't split words when soft wrapping
set linebreak

" Set fold character to blank
set fillchars="vert:|,fold:\"

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
colorscheme base16-oceanicnext
" colorscheme seoul256

" Set lightline theme
let g:lightline = {}
let g:lightline.colorscheme = 'base16_oceanicnext'

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

" Vim-rooter settings
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_manual_only = 1

" Use deoplete
let g:deoplete#enable_at_startup = 1

" Set private snippets directory
let g:UltiSnipsSnippetsDir = "~/AppData/Local/nvim/UltiSnips"

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Neoterm setup
let g:neoterm_autoscroll=1

" Denite setup
"""""""""""""""""""""""""""""""""""""""""""""""""

" Ripgrep for file_rec
call denite#custom#var('file_rec', 'command',
    \ ['rg', '--files', '--glob', '!.git', ''])

" Navigate list with Ctrl-j and Ctrl-k
call denite#custom#map(
      \ 'insert',
      \ '<C-j>',
      \ '<denite:move_to_next_line>',
      \ 'noremap'
      \)
call denite#custom#map(
      \ 'insert',
      \ '<C-k>',
      \ '<denite:move_to_previous_line>',
      \ 'noremap'
      \)

" Ripgrep command on grep source
call denite#custom#var('grep', 'command', ['rg'])
call denite#custom#var('grep', 'default_opts',
                \ ['--vimgrep', '--no-heading'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

" R
"""""""""""""""""""""""""""""""""""""""""""""""""

" Run lines and selections with Ctrl-Enter
nmap <C-Enter> <Plug>RDSendLine
vmap <C-Enter> <Plug>RDSendSelection

" Disable assignment mapping
let R_assign = 0

" Use older Rtools for Nvim-R plugin
let Rtools_path = "C:\\Rtools3.3"

" Use color scheme for R output
let rout_follow_colorscheme = 1

" Enable folding
let r_syntax_folding = 1

" Close R when quitting Vim
autocmd VimLeave * if exists("g:SendCmdToR") && string(g:SendCmdToR) != "function('SendCmdToR_fake')" | call RQuit("nosave") | endif

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
