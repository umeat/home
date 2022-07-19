set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
syntax enable
highlight clear SignColumn
set incsearch hlsearch
set nowrap
set backspace=2
set updatetime=300

call plug#begin()
 Plug 'tpope/vim-fugitive'
 Plug 'diepm/vim-rest-console'
 Plug 'junegunn/fzf'
 Plug 'itchyny/lightline.vim'
 Plug 'tpope/vim-surround'
 Plug 'tpope/vim-eunuch'
 Plug 'scrooloose/nerdtree'
 Plug 'chrisbra/Colorizer'
 Plug 'reedes/vim-wordy'
 Plug 'neoclide/coc.nvim', {'branch': 'release'}
 Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
 Plug 'LnL7/vim-nix'
 Plug 'hashivim/vim-terraform'
 Plug 'stephpy/vim-yaml'
 Plug 'joshdick/onedark.vim'
 Plug 'logico/typewriter'
 Plug 'ajgrf/parchment'
call plug#end()

colorscheme Tomorrow-Night

" fzf source and map
set rtp+=~/.vim/plugged/fzf
nnoremap <Leader>p :FZF<CR>

" vim tabs
nnoremap <Leader>t :tabnew<CR>
nnoremap <Tab> :tabnext<CR>
nnoremap <S-Tab> :tabprevious<CR>

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

" respect camelcase
let g:camelchar = "A-Z0-9.,;:{([`'\""
nnoremap <silent><C-Left> :<C-u>call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>
nnoremap <silent><C-Right> :<C-u>call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>
inoremap <silent><C-Left> <C-o>:call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>
inoremap <silent><C-Right> <C-o>:call search('\C\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>

" Jump to next COC diagnostic
nmap <silent><C-N> <Plug>(coc-diagnostic-prev)

" Sudo Write
command! -nargs=0 Sw :w !sudo tee % > /dev/null
