set nocompatible
set shell=bash
filetype off

" ========================================================================
" Vim Plug
" ========================================================================

call plug#begin('~/.vim/plugged')

" Plugins
Plug 'Lokaltog/vim-easymotion'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dockyard/vim-easydir'
Plug 'ervandew/supertab'
Plug 'janko-m/vim-test'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/gitignore'

" Syntax
Plug 'amdt/vim-niji'
Plug 'claco/jasmine.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'groenewege/vim-less'
Plug 'guns/vim-clojure-static'
Plug 'kchmck/vim-coffee-script'
Plug 'leafgarland/typescript-vim'
Plug 'mtscout6/vim-cjsx'
Plug 'mxw/vim-jsx'
Plug 'nono/vim-handlebars'
Plug 'pangloss/vim-javascript'
Plug 'slim-template/vim-slim'
Plug 'tpope/vim-cucumber'
Plug 'tpope/vim-haml'
Plug 'vim-ruby/vim-ruby'

" Syntaxhighlight tweaking
" Plug 'lilydjwg/colorizer'
" Plug 'vim-scripts/SyntaxAttr.vim'

call plug#end()

" ========================================================================
"  Settings
" ========================================================================

color sweam

set noswapfile
set number
set history=200
set scrolloff=5
set guioptions-=L
set guioptions-=r
set guioptions-=T
set backupdir=~/.vim/_backup
set directory=~/.vim/_temp
set undodir^=~/.vim/_undo/
set nobackup
set nowritebackup
set hidden
set clipboard=unnamed

" Whitespace
set nowrap
set tabstop=2
set shiftwidth=2
set expandtab
set list
set listchars=trail:⋅,nbsp:⋅,tab:▸\
set showbreak=↪

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Use Ack instead of grep
set grepprg=ag
let g:ackprg = 'ag --nogroup --column'

" Able to 'gf' files
" set suffixesadd=.rb,.coffee,.js

" (Hopefully) removes the delay when hitting esc in insert mode
set ttimeout
set ttimeoutlen=20
set notimeout
set ttyfast
set ttyscroll=5

let loaded_matchparen=1 " Don't load matchit.vim (paren/bracket matching)
set noshowmatch         " Don't match parentheses/brackets
set nocursorline        " Don't paint cursor line
set nocursorcolumn      " Don't paint cursor column
set lazyredraw          " Wait to redraw
set scrolljump=8
let html_no_rendering=1 " Don't render italic, bold, links in HTML

" View full list when tab-complete in command mode
set wildmode=list:full

" Statusline
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

" append a newline if it is not already there
set eol

map Q <Nop>

" clear the search buffer when hitting return
nnoremap <silent> <CR> :nohlsearch<cr>

" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" disable cursor keys in normal mode
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

" No difference between ; and ;
map ; :

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr>

let g:ctrlp_use_caching = 0
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:20,results:20'
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

set mouse=a
set ttymouse=xterm2

" Easymotion
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

" ========================================================================
"  Mappings
" ========================================================================

let mapleader=","

nnoremap <leader><leader> <c-^>

map <leader>A :NERDTreeFind<cr>
map <leader>F :CtrlP %%<cr>
map <leader>O :! open %%<cr><cr>
map <leader>T :TestNearest<cr>
map <leader>a :NERDTreeToggle<cr>
map <leader>b :Gblame<cr>
map <leader>cc :TComment<cr>
map <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>
map <leader>f :CtrlP<cr>
map <leader>i :%s/\t/  /g<CR> :KillWhitespace<CR>
map <leader>n :call RenameFile()<cr>
map <leader>o :! open .<cr><cr>
map <leader>q :bd<CR>
map <leader>t :TestFile<cr>
map <leader>v :tabe $MYVIMRC<CR>
map <leader>w :bp<CR>:bd#<CR>
map <leader>x :bn<CR>
map <leader>z :bp<CR>

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

map <C-n> :cn<CR>
map <C-p> :cp<CR>

nmap <Space><Space> <Plug>(easymotion-s)

" ========================================================================
"  Autocmd
" ========================================================================

if has("autocmd")

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g`\"" | endif

  " Make sure js.coffee and js.cjsx is jasmine
  au BufRead,BufNewFile *[Ss]pec.js.coffee, set filetype=jasmine.coffee
  au BufRead,BufNewFile *test.{js.coffee,js.cjsx,cjsx,coffee}, set filetype=jasmine.coffee

  " Treat JSON files like JavaScript
  au BufRead,BufNewFile *.json set ft=javascript

  " Treat ERB as ruby erb file
  au BufRead,BufNewFile *.{skim,slim} set filetype=slim
  au BufRead,BufNewFile *.erb set filetype=eruby.html

  " Trim whitespace
  au FileWritePre,FileAppendPre,FilterWritePre,BufWritePre * :call TrimWhiteSpace()

  " Reload vimrc on save
  augroup reload_vimrc " {
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
  augroup END " }
endif

" ========================================================================
"  Functions
" ========================================================================

function! TrimWhiteSpace()
  %s/\s*$//
  ''
endfunction

function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
