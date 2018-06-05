set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
syntax enable
colorscheme pablo 
set incsearch hlsearch
set nowrap
set backspace=2

call plug#begin()
 Plug 'diepm/vim-rest-console'
 Plug 'junegunn/fzf'
 Plug 'itchyny/lightline.vim'
 Plug '~/bin/eclim'
 Plug 'tpope/vim-fugitive'
 Plug 'tpope/vim-surround'
 Plug 'tpope/vim-eunuch'
 Plug 'scrooloose/nerdtree'
call plug#end()

" fzf source and map
set rtp+=~/.vim/plugged/fzf
nnoremap <Leader>p :FZF<CR>

" Nul is C-Space
" switch between splits
map <Nul> <C-w>w

" lightline scheme
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ }

" switch back-and-forth between Code.java and CodeTest.java - can't define in
" ftplugin/java.vim for some reason
function! Test()
    if @% =~ 'Test.java$'
        let l:test = substitute(substitute(@%, '/test/', '/main/', ''), 'Test.java$', '.java', '')
    else
        let l:test = substitute(substitute(@%, '/main/', '/test/', ''), '.java$', 'Test.java', '')
    endif
    execute 'edit' l:test
endfunction

:command! Test :call Test()
