set nocompatible
filetype off

" ========================================================================
" Vundle stuff
" ========================================================================
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'ervandew/supertab'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tomtom/tcomment_vim'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-ruby/vim-ruby'
Bundle 'mileszs/ack.vim'
Bundle 'wincent/Command-T'

" https://github.com/ecomba/vim-ruby-refactoring

" ========================================================================
"  Settings
" ========================================================================

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

" Able to 'gf' files
set suffixesadd=.rb,.coffee,.js

" (Hopefully) removes the delay when hitting esc in insert mode
set noesckeys
set ttimeout
set ttimeoutlen=1

" View full list when tab-complete in command mode
set wildmode=list:full

" Statusline
set laststatus=2
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

" When at 3 spaces and I hit >>, go to 4, not 5.
set shiftround

" don't use Ex mode, use Q for formatting
map Q gq

" clear the search buffer when hitting return
nnoremap <silent> <CR> :nohlsearch<cr>

" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" ignore Rubinius, Sass cache files
set wildignore+=*/tmp/**,*.rbc,.rbx,*.scssc,*.sassc,*/vendor/**

" disable cursor keys in normal mode
map <Left>  :echo "no!"<cr>
map <Right> :echo "no!"<cr>
map <Up>    :echo "no!"<cr>
map <Down>  :echo "no!"<cr>

" No difference between ; and ;
map ; :

command! KillWhitespace :normal :%s/ *$//g<cr><c-o><cr>

let g:CommandTMaxHeight=10
let g:CommandTMinHeight=4

let mapleader=","

" ========================================================================
"  Mappings
" ========================================================================

nnoremap <leader><leader> <c-^>
map <leader>b :CommandTBuffer<cr>
map <leader>f :CommandT<cr>
map <leader>F :CommandT %%<cr>
map <leader>.v :CommandT app/views<cr>
map <leader>.c :CommandT app/controllers<cr>
map <leader>.m :CommandT app/models<cr>
map <leader>.h :CommandT app/helpers<cr>
map <leader>.k :CommandT config<cr>
map <leader>.l :CommandT lib<cr>
map <leader>.r :topleft :split config/routes.rb<cr>
map <leader>.j :CommandT app/assets/javascripts<cr>
map <leader>.s :CommandT app/assets/stylesheets<cr>
map <leader>/t :CommandT app/assets/javascripts/templates<cr>
map <leader>/m :CommandT app/assets/javascripts/models<cr>
map <leader>/v :CommandT app/assets/javascripts/views<cr>
map <leader>/c :CommandT app/assets/javascripts/views<cr>
map <leader>/r :topleft :split app/assets/javascripts/router.js.coffee<cr>

" find merge conflict markers
nmap <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Clear whitespace
nmap <leader>W :KillWhitespace<CR>

" Easy commenting
map <leader>cc :TComment<cr>

" Make editing vimrc simple
nmap <leader>v :e $MYVIMRC<CR>

" Window management
map <Leader>z :bp<CR>
map <Leader>x :bn<CR>
map <Leader>q :bd<CR>
map <Leader>w :bp<CR>:bd#<CR>

" Open current dir in finder
nmap <leader>o :! open .<cr><cr>
nmap <leader>O :! open %%<cr><cr>

" Tests
map <Leader>T :call RunCurrentTest()<CR>
map <Leader>t :call RunCurrentLineInTest()<CR>
map <Leader>u :Runittest<cr>

"Rename File
map <leader>n :call RenameFile()<cr>

" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

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
      call SetTestRunner("!cucumber")
      exec g:bjo_test_runner g:bjo_test_file
    elseif match(expand('%'), '_spec\.rb$') != -1
      call SetTestRunner("!bundle exec rspec")
      exec g:bjo_test_runner g:bjo_test_file
    else
      call SetTestRunner("!ruby -Itest")
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

  exec "!bundle exec rspec" g:bjo_test_file . ":" . g:bjo_test_file_line
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

function! OpenFactoryFile()
  if filereadable("test/factories.rb")
    execute ":sp test/factories.rb"
  else
    execute ":sp spec/factories.rb"
  end
endfunction
