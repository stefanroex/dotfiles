if !has('nvim')
  set nocompatible
endif
set shell=bash
filetype off

if has('nvim')
  language en_US
endif

" ========================================================================
" Vim Plug
" ========================================================================

call plug#begin('~/.vim/plugged')

" Plugins
" Plug 'Galooshi/vim-import-js'
Plug 'Lokaltog/vim-easymotion'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dockyard/vim-easydir'
Plug 'ervandew/supertab'
Plug 'janko-m/vim-test'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'sbdchd/neoformat'
Plug 'scrooloose/nerdtree'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/gitignore'
Plug 'w0ng/vim-hybrid'
Plug 'w0rp/ale'
Plug 'flowtype/vim-flow'

if !has('nvim')
  Plug 'tpope/vim-sensible'
endif

" Syntax
Plug 'JulesWang/css.vim'
Plug 'claco/jasmine.vim'
Plug 'cyberkov/openhab-vim'
Plug 'groenewege/vim-less'
Plug 'guns/vim-clojure-static'
Plug 'kchmck/vim-coffee-script'
Plug 'mtscout6/vim-cjsx'
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'slim-template/vim-slim'
Plug 'tpope/vim-cucumber'
Plug 'tpope/vim-haml'
Plug 'vim-ruby/vim-ruby'
Plug 'hachibeeDI/vim-vbnet'

" Syntaxhighlight tweaking
" Plug 'lilydjwg/colorizer'
" Plug 'vim-scripts/SyntaxAttr.vim'

call plug#end()

" ========================================================================
"  Settings
" ========================================================================


" Theme
set background=dark
colorscheme sweam

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

if !has('nvim')
  set ttyfast
  set ttyscroll=5
endif

let loaded_matchparen=1 " Don't load matchit.vim (paren/bracket matching)
set noshowmatch         " Don't match parentheses/brackets
set nocursorline        " Don't paint cursor line
set nocursorcolumn      " Don't paint cursor column
set lazyredraw          " Wait to redraw
set scrolljump=8
let html_no_rendering=1 " Don't render italic, bold, links in HTML

" View full list when tab-complete in command mode
set wildmode=list:full

" Javascript settings
let g:javascript_enable_domhtmlcss = 1
let g:javascript_plugin_flow = 0
let g:jsx_ext_required = 0

" Statusline
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

" append a newline if it is not already there
set eol

map Q @q

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
" let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']

augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

set mouse=a

if !has('nvim')
  set ttymouse=xterm2
endif

" Easymotion
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

" Vim-test
let test#javascript#mocha#executable = 'node_modules/.bin/mocha --compilers js:babel-core/register --require ./test/testHelper.js'

let g:terminal_color_0  = '#2e3436'
let g:terminal_color_1  = '#cc0000'
let g:terminal_color_2  = '#4e9a06'
let g:terminal_color_3  = '#c4a000'
let g:terminal_color_4  = '#3465a4'
let g:terminal_color_5  = '#75507b'
let g:terminal_color_6  = '#0b939b'
let g:terminal_color_7  = '#d3d7cf'
let g:terminal_color_8  = '#555753'
let g:terminal_color_9  = '#ef2929'
let g:terminal_color_10 = '#8ae234'
let g:terminal_color_11 = '#fce94f'
let g:terminal_color_12 = '#729fcf'
let g:terminal_color_13 = '#ad7fa8'
let g:terminal_color_14 = '#00f5e9'
let g:terminal_color_15 = '#eeeeec'

" Vim Flow
let g:flow#enable = 0
let g:flow#autoclose = 1

" Prettier
" autocmd FileType javascript setlocal formatprg=prettier\ --stdin\ --trailing-comma\ all
" let g:neoformat_try_formatprg = 1

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
nnoremap <silent> ,d :call neoterm#close()<cr>
map <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>
map <leader>f :CtrlP<cr>
" map <leader>i :%s/\t/  /g<CR> :KillWhitespace<CR>
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

" Move cursor to end of passed text
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

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
  au BufRead,BufNewFile *.css set filetype=scss

  " Trim whitespace
  " au FileWritePre,FileAppendPre,FilterWritePre,BufWritePre * :call TrimWhiteSpace()

  " Reload vimrc on save
  augroup reload_vimrc " {
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
  augroup END " }

  " autocmd BufWritePre *.js Neoformat
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
