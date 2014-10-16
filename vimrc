set nocompatible
filetype off

" ========================================================================
" Vim Plug
" ========================================================================

call plug#begin('~/.vim/plugged')

" Plugins
Plug 'SirVer/ultisnips'
Plug 'dockyard/vim-easydir'
Plug 'ervandew/supertab'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-ruby/vim-ruby'
Plug 'vim-scripts/VimClojure'

" Syntax
Plug 'elixir-lang/vim-elixir'
Plug 'groenewege/vim-less'
Plug 'heartsentwined/vim-ember-script'
Plug 'heartsentwined/vim-emblem'
Plug 'kchmck/vim-coffee-script'
Plug 'mxw/vim-jsx'
Plug 'nono/vim-handlebars'
Plug 'slim-template/vim-slim'
Plug 'tpope/vim-cucumber'
Plug 'tpope/vim-haml'
Plug 'mtscout6/vim-cjsx'

call plug#end()

" ========================================================================
"  Settings
" ========================================================================

color sweam

set noswapfile
" set rnu
set number
" set cursorline
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

" Windows
" set winwidth=84
" set winheight=10
" set winminheight=10
" set winheight=999

" Use Ack instead of grep
set grepprg=ag
let g:ackprg = 'ag --nogroup --column'

" Able to 'gf' files
set suffixesadd=.rb,.coffee,.js

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
map K <Nop>

" clear the search buffer when hitting return
nnoremap <silent> <CR> :nohlsearch<cr>

" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" ignore Rubinius, Sass cache files
set wildignore+=*/tmp/**,*/bin/**,*.rbc,.rbx,*.scssc,*.sassc,*/vendor/**,/node_modules/**,/components/**,build

" disable cursor keys in normal mode
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

" No difference between ; and ;
map ; :

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr>

let g:ctrlp_extensions = ['funky']
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:30,results:30'
let g:ctrlp_use_caching = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

set mouse=a
set ttymouse=xterm2

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" ========================================================================
"  Mappings
" ========================================================================

let mapleader=","

nnoremap <leader><leader> <c-^>

nnoremap K :Ag "\b<C-R><C-W>\b"<CR>:cw<CR>

map <leader>.c :CtrlP app/controllers<cr>
map <leader>.h :CtrlP app/helpers<cr>
map <leader>.j :CtrlP app/assets/javascripts<cr>
map <leader>.k :CtrlP config<cr>
map <leader>.l :CtrlP lib<cr>
map <leader>.m :CtrlP app/models<cr>
map <leader>.r :topleft :split config/routes.rb<cr>
map <leader>.s :CtrlP app/assets/stylesheets<cr>
map <leader>.t :CtrlP spec<cr>
map <leader>.v :CtrlP app/views<cr>
map <leader>/c :CtrlP app/assets/javascripts/views<cr>
map <leader>/m :CtrlP app/assets/javascripts/models<cr>
map <leader>/r :topleft :split app/assets/javascripts/router.js.coffee<cr>
map <leader>/t :CtrlP app/assets/javascripts/templates<cr>
map <leader>/v :CtrlP app/assets/javascripts/views<cr>
map <leader>A :NERDTreeFind<cr>
map <leader>F :CtrlP %%<cr>
map <leader>O :! open %%<cr><cr>
map <leader>T :call RunCurrentLineInTest()<CR>
map <leader>W :KillWhitespace<CR>
map <leader>a :NERDTreeToggle<cr>
map <leader>b :Gblame<cr>
map <leader>cc :TComment<cr>
map <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>
map <leader>f :CtrlP<cr>
map <Leader>Fu :CtrlPFunky<Cr>
map <Leader>FU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
map <leader>i :%s/\t/  /g<CR> :KillWhitespace<CR>
map <leader>n :call RenameFile()<cr>
map <leader>o :! open .<cr><cr>
map <leader>r :!rspec<cr>
map <leader>q :bd<CR>
map <leader>t :call RunCurrentTest()<CR>
map <leader>v :tabe $MYVIMRC<CR>
map <leader>w :bp<CR>:bd#<CR>
map <leader>x :bn<CR>
map <leader>y :call <SID>SynStack()<CR>
map <leader>z :bp<CR>

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

imap <C-l> :<Space>
map <C-s> <esc>:w<CR>
imap <C-s> <esc>:w<CR>
map <C-t> <esc>:tabnew<CR>
map <C-x> <C-w>c
map <C-n> :cn<CR>
map <C-p> :cp<CR>

" Emacs-like beginning and end of line.
imap <c-e> <c-o>$
imap <c-a> <c-o>^

" ========================================================================
"  Autocmd
" ========================================================================


if has("autocmd")
  " Treat JSON files like JavaScript
  au BufNewFile,BufRead *.json set ft=javascript

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

  " mark Jekyll YAML frontmatter as comment
  au BufNewFile,BufRead *.{md,markdown,html,xml} sy match Comment /\%^---\_.\{-}---$/

  " Treat ERB as ruby erb file
  au BufRead,BufNewFile *.skim set filetype=slim
  au BufRead,BufNewFile *.slim set filetype=slim
  au BufRead,BufNewFile *.erb set filetype=eruby.html

  " Trim whitespace
  autocmd FileWritePre * :call TrimWhiteSpace()
  autocmd FileAppendPre * :call TrimWhiteSpace()
  autocmd FilterWritePre * :call TrimWhiteSpace()
  autocmd BufWritePre * :call TrimWhiteSpace()

  " Reload vimrc on save
  au BufWritePost .vimrc source $MYVIMRC
endif

" ========================================================================
"  Functions
" ========================================================================

function! TrimWhiteSpace()
  %s/\s*$//
  ''
:endfunction

function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

function! RunCurrentTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\|_spec.js.coffee\)$') != -1

  if in_test_file
    call SetTestFile()

    if match(expand('%'), '\.feature$') != -1
      call SetTestRunner("!cucumber")
    elseif match(expand('%'), '_spec\.rb$') != -1
      call SetTestRunner("!bin/rspec")
    elseif match(expand('%'), '_spec\.js\.coffee$') != -1
      call SetTestRunner("!bin/teaspoon")
    else
      call SetTestRunner("!ruby -Itest")
    endif
  endif

  exec "w " g:bjo_test_file
  exec g:bjo_test_runner g:bjo_test_file
endfunction

function! SetTestRunner(runner)
  let g:bjo_test_runner=a:runner
endfunction

function! RunCurrentLineInTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFileWithLine()
  end

  exec "w " g:bjo_test_file
  exec "!rspec" g:bjo_test_file . ":" . g:bjo_test_file_line
endfunction

function! SetTestFile()
  let g:bjo_test_file=@%
endfunction

function! SetTestFileWithLine()
  let g:bjo_test_file=@%
  let g:bjo_test_file_line=line(".")
endfunction

function! CorrectTestRunner()
  if match(expand('%'), '\.feature$') != -1
    return "cucumber"
  elseif match(expand('%'), '_spec\.rb$') != -1
    return "rspec"
  elseif match(expand('%'), '_spec\.js\.coffee$') != -1
    return "js"
  else
    return "ruby"
  endif
endfunction
