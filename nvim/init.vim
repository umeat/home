set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
syntax enable
colorscheme default 
highlight clear SignColumn
set incsearch hlsearch
set nowrap
set backspace=2
set updatetime=300

inoremap <silent><expr> <cr> coc#pum#visible() ? coc#_select_confirm() : "\<C-g>u\<CR>"

call plug#begin('~/.vim/plugged')
 Plug 'diepm/vim-rest-console'
 Plug 'junegunn/fzf'
 Plug 'itchyny/lightline.vim'
 Plug '~/bin/eclim'
 Plug 'tpope/vim-fugitive'
 Plug 'tpope/vim-surround'
 Plug 'tpope/vim-eunuch'
 Plug 'scrooloose/nerdtree'
 Plug 'chrisbra/Colorizer'
 Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
 Plug 'neoclide/coc.nvim', {'branch': 'release'}
 Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
call plug#end()

" fzf source and map
set rtp+=~/.vim/plugged/fzf
nnoremap <Leader>p :FZF<CR>

" eclim
let g:EclimLoggingDisabled = 1
let g:EclimMakeLCD = 1
set completeopt-=preview

" vim tabs
map <C-t> :tabnew<CR>
map <Tab> :tabnext<CR>
map <S-Tab> :tabprevious<CR>

" Nul is C-Space
" switch between splits
map <Nul> <C-w>w

" search highlighting
nnoremap <Leader>/ :noh<CR>

" lightline scheme
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ }

:command! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
