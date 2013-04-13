set nocompatible
filetype off

" ========================================================================
" Vundle stuff
" ========================================================================
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Plugins
NeoBundle 'wincent/Command-T'
NeoBundle 'ervandew/supertab'
NeoBundle 'mileszs/ack.vim'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-sensible'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'scrooloose/nerdtree'

" Syntax
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'nono/vim-handlebars'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'tpope/vim-cucumber'
NeoBundle 'tpope/vim-haml'

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundleCheck

" ========================================================================
"  Settings
" ========================================================================

color railscasts
set number
set cursorline
set history=200
set guioptions-=L
set guioptions-=r
set guioptions-=T
set backupdir=~/.vim/_backup
set directory=~/.vim/_temp
set undodir^=~/.vim/undo/
set nobackup
set nowritebackup
set hidden

set ttyfast " u got a fast terminal
set ttyscroll=3
set lazyredraw " to avoid scrolling problems

" Whitespace
set nowrap
set tabstop=2
set shiftwidth=2
set expandtab
set list
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

" Able to 'gf' files
set suffixesadd=.rb,.coffee,.js

" (Hopefully) removes the delay when hitting esc in insert mode
set noesckeys
set ttimeout
set ttimeoutlen=1

" View full list when tab-complete in command mode
set wildmode=list:full

" Statusline
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

map Q <Nop>
map K <Nop>

" clear the search buffer when hitting return
nnoremap <silent> <CR> :nohlsearch<cr>

" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" ignore Rubinius, Sass cache files
set wildignore+=*/tmp/**,*.rbc,.rbx,*.scssc,*.sassc,*/vendor/**,node_modules

" disable cursor keys in normal mode
map <Left> <Nop>
map <Right> <Nop>a
map <Up> <Nop>
map <Down> <Nop>

" No difference between ; and ;
map ; :

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr>

let g:CommandTCancelMap=['<ESC>','<C-c>']

augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

" ========================================================================
"  Mappings
" ========================================================================

let mapleader=","

nnoremap <leader><leader> <c-^>

map <leader>.c :CommandT app/controllers<cr>
map <leader>.h :CommandT app/helpers<cr>
map <leader>.j :CommandT app/assets/javascripts<cr>
map <leader>.k :CommandT config<cr>
map <leader>.l :CommandT lib<cr>
map <leader>.m :CommandT app/models<cr>
map <leader>.r :topleft :split config/routes.rb<cr>
map <leader>.s :CommandT app/assets/stylesheets<cr>
map <leader>.t :CommandT spec<cr>
map <leader>.v :CommandT app/views<cr>
map <leader>/c :CommandT app/assets/javascripts/views<cr>
map <leader>/m :CommandT app/assets/javascripts/models<cr>
map <leader>/r :topleft :split app/assets/javascripts/router.js.coffee<cr>
map <leader>/t :CommandT app/assets/javascripts/templates<cr>
map <leader>/v :CommandT app/assets/javascripts/views<cr>
map <leader>A :NERDTreeFind<cr>
map <leader>F :CommandT %%<cr>
map <leader>O :! open %%<cr><cr>
map <leader>T :call RunCurrentLineInTest()<CR>
map <leader>W :KillWhitespace<CR>
map <leader>a :NERDTreeToggle<cr>
map <leader>b :Gblame<cr>
map <leader>cc :TComment<cr>
map <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>
map <leader>f :CommandT<cr>
map <leader>i :%s/\t/  /g<CR> :KillWhitespace<CR>
map <leader>n :call RenameFile()<cr>
map <leader>o :! open .<cr><cr>
map <leader>q :bd<CR>
map <leader>t :call RunCurrentTest()<CR>
map <leader>v :tabe $MYVIMRC<CR>
map <leader>w :bp<CR>:bd#<CR>
map <leader>x :bn<CR>
map <leader>z :bp<CR>

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

map <C-h> :nohl<cr>
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
  au BufRead,BufNewFile *.erb set filetype=eruby.html

  " Treat ERB as ruby erb file
  au BufRead,BufNewFile *.skim set filetype=slim

  " Reload vimrc on save
  au BufWritePost .vimrc source $MYVIMRC

  " Flush CommandT automaticaly
  au FocusGained,BufWritePost * CommandTFlush
endif


" ========================================================================
"  Functions
" ========================================================================

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
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFile()

    if match(expand('%'), '\.feature$') != -1
      call SetTestRunner("ConqueTermSplit cucumber")
      exec g:bjo_test_runner g:bjo_test_file
    elseif match(expand('%'), '_spec\.rb$') != -1
      call SetTestRunner("!rspec --no-color")
      exec g:bjo_test_runner g:bjo_test_file
    else
      call SetTestRunner("ConqueTermSplit ruby -Itest")
      exec g:bjo_test_runner g:bjo_test_file
    endif
  else
    exec g:bjo_test_runner g:bjo_test_file
  endif
endfunction

function! SetTestRunner(runner)
  let g:bjo_test_runner=a:runner
endfunction

function! RunCurrentLineInTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFileWithLine()
  end

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
  else
    return "ruby"
  endif
endfunction
