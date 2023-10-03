set nocompatible
set encoding=utf-8

" -----------------------
" Plugins using vim-plug.
" -----------------------

call plug#begin('~/.vim/plugged')

Plug 'gruvbox-community/gruvbox' " Gruvbox theme
Plug 'sainnhe/everforest' " Everforest theme
Plug 'vim-airline/vim-airline' " Status bar
Plug 'lervag/vimtex' " Latex support
Plug 'SirVer/ultisnips' " Snippets
Plug 'cloudhead/neovim-fuzzy' " Fuzzy file search
Plug 'vimwiki/vimwiki' " Notes

call plug#end()

" ---------------------
" Plugin configuration.
" ---------------------

set termguicolors

" --- gruvbox ---
set background=dark
let g:gruvbox_contrast_dark = 'medium'
colorscheme gruvbox

" --- everforest ---
" set background=dark
" let g:everforest_background = 'hard'
" let g:everforest_better_performance = 1
" colorscheme everforest


" --- vim-airline ---
let g:airline_powerline_fonts = 1

" --- vimtex ---
let maplocalleader = "\<Space>"
let g:vimtex_quickfix_mode = 0

" Put build files in a folder.
let g:vimtex_compiler_latexmk = {
  \ 'build_dir' : 'build_dir',
  \}
let g:vimtex_indent_enabled   = 0

" For the metropolis beamer theme.
" let g:vimtex_compiler_latexmk_engines = {
"   \ '_' : '-lualatex',
"   \}

" --- ultisnips ---
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" --- neovim-fuzzy ---
" Ctrl-/ to trigger search.
nnoremap <silent> <C-_> :FuzzyOpen<CR>

" --- vimwiki ---
let g:vimwiki_list = [{'path': '~/Documents/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]
let g:vimwiki_global_ext = 0
let g:vimwiki_markdown_link_ext = 1

" ---------------
" Other settings.
" ---------------

" Keep the cursor at the center of the screen.
set scrolloff=100

" Make backspace work proper.
set backspace=indent,eol,start

" Enable file specific plugins and syntax highlighting.
filetype indent plugin on
syntax on

set number " Show line numbers.
set showcmd " Show commands.
set cursorline " Highlight the line with the cursor.

" Enable all python syntax highlighting features.
let g:python_highlight_all = 1

" Enable smart search.
set ignorecase
set smartcase

" No backup and swap files.
set nobackup noswapfile

" Needed for neovim.
let g:python3_host_prog = '/usr/bin/python3'

" 2 spaces are used for indentation by default.
set tabstop=2
set softtabstop=2
set shiftwidth=2
set smarttab
set expandtab

" ------------
" Keybindings.
" ------------

" Set leader key to spacebar.
nnoremap <Space> <nop>
let mapleader = "\<Space>"

" Map jk to esc key in insert mode.
inoremap jk <ESC>

" Map semicolon to colon.
nnoremap ; :

" Split navigation.
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Tab navigation.
nnoremap <silent> <C-n> :tabnext<CR>
nnoremap <silent> <C-p> :tabprevious<CR>

" Move vertically by visual line.
nnoremap j gj
nnoremap k gk

" Copy to and paste from the system clipboard.
vnoremap <C-c> "+y<ESC>
vnoremap <C-v> c<ESC>"+p

" Toggle spellcheck.
nnoremap <silent> <leader>s :setlocal spell! spelllang=en_us<CR>
