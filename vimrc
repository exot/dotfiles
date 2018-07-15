if filereadable("/etc/vim/vimrc")
  so /etc/vim/vimrc
endif

set modeline
set showcmd
set showmatch
set shiftwidth=2
set nopaste
set expandtab
set autoindent
set number

syntax enable
set background=light
filetype plugin indent on

let g:tex_flavor = "latex"
let b:is_mzscheme = 1

set spellfile="~/.vim/spell"

map Q gq
imap jj <ESC>

let clj_highlight_builtins = 1
let clj_highlight_contrib = 1
let clj_want_gorilla = 1


