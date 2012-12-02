set nocompatible
filetype off

" ========================================================================
" Vundle stuff
" ========================================================================
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" My bundles
Bundle 'ervandew/supertab'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tomtom/tcomment_vim'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-ruby/vim-ruby'
Bundle 'wincent/Command-T'
Bundle 'mileszs/ack.vim'

colorscheme railscasts

syntax enable
filetype plugin indent on
set number
set ruler
set cursorline
set history=200
set scrolloff=5
set guioptions-=L
set guioptions-=r
set guioptions-=T
set backupdir=~/.vim/_backup
set directory=~/.vim/_temp
set nobackup
set nowritebackup
set hidden

"" Whitespace
set nowrap
set tabstop=2
set shiftwidth=2
set expandtab
set list
set backspace=indent,eol,start
set listchars=trail:⋅,nbsp:⋅,tab:▸\

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Windows
set winwidth=84
set winheight=10
set winminheight=10
set winheight=999

" Use Ack instead of grep
set grepprg=ack

" (Hopefully) removes the delay when hitting esc in insert mode
set noesckeys
set ttimeout
set ttimeoutlen=1

" Set up some useful Rails.vim bindings for working with Backbone.js
autocmd User Rails Rnavcommand template    app/assets/templates               -glob=**/*  -suffix=.jst.ejs
autocmd User Rails Rnavcommand jmodel      app/assets/javascripts/models      -glob=**/*  -suffix=.coffee
autocmd User Rails Rnavcommand jview       app/assets/javascripts/views       -glob=**/*  -suffix=.coffee
autocmd User Rails Rnavcommand jcollection app/assets/javascripts/collections -glob=**/*  -suffix=.coffee
autocmd User Rails Rnavcommand jrouter     app/assets/javascripts/routers     -glob=**/*  -suffix=.coffee
autocmd User Rails Rnavcommand jspec       spec/javascripts                   -glob=**/*  -suffix=.coffee

" don't use Ex mode, use Q for formatting
map Q gq

" clear the search buffer when hitting return
:nnoremap <silent> <CR> :nohlsearch<cr>

let mapleader=","

" paste lines from unnamed register and fix indentation
nmap <leader>p pV`]=
nmap <leader>P PV`]=

map <leader>gr :topleft :split config/routes.rb<cr>
map <leader>gv :CommandTFlush<cr>\|:CommandT app/views<cr>
map <leader>gc :CommandTFlush<cr>\|:CommandT app/controllers<cr>
map <leader>gm :CommandTFlush<cr>\|:CommandT app/models<cr>
map <leader>gh :CommandTFlush<cr>\|:CommandT app/helpers<cr>
map <leader>gl :CommandTFlush<cr>\|:CommandT lib<cr>
map <leader>gf :CommandTFlush<cr>\|:CommandT features<cr>
map <leader>gg :topleft 100 :split Gemfile<cr>
map <leader>f :CommandTFlush<cr>\|:CommandT<cr>
" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>F :CommandTFlush<cr>\|:CommandT %%<cr>

let g:CommandTMaxHeight=10
let g:CommandTMinHeight=4

" ignore Rubinius, Sass cache files
set wildignore+=tmp/**,*.rbc,.rbx,*.scssc,*.sassc,vendor/**

nnoremap <leader><leader> <c-^>

" find merge conflict markers
nmap <silent> <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr>
nmap <silent> <leader>W :KillWhitespace<CR>

" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" disable cursor keys in normal mode
map <Left>  :echo "no!"<cr>
map <Right> :echo "no!"<cr>
map <Up>    :echo "no!"<cr>
map <Down>  :echo "no!"<cr>

set laststatus=2
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

" Make editing vimrc simple
nmap <leader>v :e $MYVIMRC<CR>

map <silent> <Leader>z :bp<CR>
map <silent> <Leader>x :bn<CR>
map <silent> <Leader>q :bd<CR>
map <silent> <Leader>w :bp<CR>:bd#<CR>
imap <c-l> <space>=><space>

" Open current dir in finder
nmap <silent> <Leader>o :! open .<CR><CR>
nmap <silent> <Leader>O :! open %%<CR><CR>

" Can't be bothered to understand ESC vs <c-c> in insert mode
map <c-c> <esc>

autocmd BufRead,BufNewFile *.erb set filetype=eruby.html

if has("autocmd")
  " Treat JSON files like JavaScript
  au BufNewFile,BufRead *.json set ft=javascript

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

  " mark Jekyll YAML frontmatter as comment
  au BufNewFile,BufRead *.{md,markdown,html,xml} sy match Comment /\%^---\_.\{-}---$/
endif
