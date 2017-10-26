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
Plug 'roxma/nvim-completion-manager'

" Fuzzy finder
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Linter
Plug 'w0rp/ale'

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

" Comment
Plug 'tpope/vim-commentary'

" Faster folding
Plug 'Konfekt/FastFold'

" Git integration
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'

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

" Better window splitting
set splitright
set splitbelow

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

" Set private snippets directory
let g:UltiSnipsSnippetsDir = "~/AppData/Local/nvim/UltiSnips"

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Neoterm setup
let g:neoterm_autoscroll=1

" Fzf setup
"""""""""""""""""""""""""""""""""""""""""""""""""

" Search
nnoremap <leader>b :<C-u>Buffer<CR>
nnoremap <leader>f :<C-u>Files<CR>

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

" Enable vimtex autocomplete with NCM
augroup my_cm_setup
autocmd!
autocmd User CmSetup call cm#register_source({
      \ 'name' : 'vimtex',
      \ 'priority': 8,
      \ 'scoping': 1,
      \ 'scopes': ['tex'],
      \ 'abbreviation': 'tex',
      \ 'cm_refresh_patterns': g:vimtex#re#ncm,
      \ 'cm_refresh': {'omnifunc': 'vimtex#complete#omnifunc'},
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
"let g:vimtex_view_general_options
"	\ = '-reuse-instance -forward-search @tex @line @pdf'
let g:vimtex_view_general_options
	\ = '@pdf'
let g:vimtex_view_general_options_latexmk = '-reuse-instance'


""""""""""""""""
