set nocompatible

"Plugins using vim-plug
call plug#begin('~/.vim/plugged')

Plug 'lervag/vimtex' "Latex support
Plug 'SirVer/ultisnips' "Snippets
Plug 'cloudhead/neovim-fuzzy' "Fuzzy file search

call plug#end()

"Set encoding
set enc=utf-8

"Vimtex
let g:vimtex_compiler_latexmk = {
	\ 'build_dir' : 'build_dir',
	\}

"UltiSnips
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

"Keep cursor at the center of the screen
set scrolloff=20

"Set 256 colors
set t_Co=256

"Theme settings
let g:gruvbox_contrast_dark = 'dark'
colorscheme gruvbox
set background=dark

"Make backspace work proper
set backspace=indent,eol,start

"Enabling file specific plugins and syntax highlighting
filetype indent plugin on
syntax on

"Show line numbers
set number

"Enable all python syntax highlighting features
let g:python_highlight_all = 1

"Remap esc key in insert mode
inoremap jk <ESC>

"Remap semicolon to colon
nnoremap ; :

"Set leader key
nnoremap <Space> <nop>
let mapleader = "\<Space>"

"Split navigations
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Enabling smart search
set ignorecase
set smartcase

set showcmd "Show commands
set cursorline "Highlight the line with cursor

"Move vertically by visual line
nnoremap j gj
nnoremap k gk

"Copy to the system clipboard
"Only works in visual mode
vmap <C-c> "+y<ESC>
vmap <C-v> c<ESC>"+p

"Stop backup and swap files
set nobackup noswapfile

"Needed for neovim
let g:python3_host_prog = '/usr/bin/python3'

"Fuzzy file search (Ctrl-/)
nnoremap <C-_> :FuzzyOpen<CR>

