set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
syntax enable
colorscheme pablo 
highlight clear SignColumn
set incsearch hlsearch
set nowrap
set backspace=2
set updatetime=300

call plug#begin()
 Plug 'diepm/vim-rest-console'
 Plug 'junegunn/fzf'
 Plug 'itchyny/lightline.vim'
 Plug 'tpope/vim-fugitive'
 Plug 'tpope/vim-surround'
 Plug 'tpope/vim-eunuch'
 Plug 'scrooloose/nerdtree'
 Plug 'chrisbra/Colorizer'
 Plug 'reedes/vim-wordy'
 Plug 'neoclide/coc.nvim', {'tag': '*', 'do': './install.sh'}
call plug#end()

" yank to clipboard
map Y "+y

" fzf source and map
set rtp+=~/.vim/plugged/fzf
nnoremap <Leader>p :FZF<CR>

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

:command! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"

"function! s:check_back_space() abort
"  let col = col('.') - 1
"  return !col || getline('.')[col - 1]  =~ '\s'
"endfunction
"
"inoremap <silent><expr> <Tab>
"      \ pumvisible() ? "\<C-n>" :
"      \ <SID>check_back_space() ? "\<Tab>" :
"      \ coc#refresh()
