set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
syntax enable
colorscheme pablo 
set incsearch hlsearch
set nowrap

call plug#begin()
 Plug 'diepm/vim-rest-console'
 Plug 'junegunn/fzf'
 Plug 'itchyny/lightline.vim'
 Plug '~/bin/eclim'
 Plug 'tpope/vim-fugitive'
call plug#end()

set rtp+=~/.vim/plugged/fzf
nnoremap <Leader>p :FZF<CR>

" Nul is C-Space
map <Nul> <C-w>w

set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ }
