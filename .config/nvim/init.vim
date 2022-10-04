""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle set up

set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'VundleVim/Vundle.vim'

Plugin 'morhetz/gruvbox'

Plugin 'kien/rainbow_parentheses.vim'

call vundle#end()
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" gruvbox configuration

let g:gruvbox_italic=1
let g:gruvbox_transparent_bg=1

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

autocmd vimenter * ++nested colorscheme gruvbox

" Fix bg transparency
" https://github.com/morhetz/gruvbox/issues/375
autocmd vimenter * hi Normal ctermbg=NONE guibg=NONE

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rainbow parentheses
" autocmd vimenter * RainbowParenthesesActivate
" autocmd vimenter * RainbowParenthesesLoadRound

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Other

" spaces > tabs
" TODO: Make exception for makefiles
set expandtab
set smarttab
set tabstop=4
set shiftwidth=4

" search options
set hlsearch
set incsearch
set ignorecase
set smartcase

" Hybrid numbering
set number relativenumber

" Invisible characters
set list
set listchars=tab:>\ ,space:·,trail:!,eol:¬

" Exit terminal mode with <Esc>
tnoremap <Esc> <C-\><C-n>

" Rebind window button to space
nnoremap <Space> <C-W>

" Pseudo maximize
" broken
" nnoremap <C-W><M> <C-W><_><C-W><C-|>

" Strip trailing whitespace on save
" https://stackoverflow.com/questions/356126/how-can-you-automatically-remove-trailing-whitespace-in-vimutocmd
function! <SID>StripTrailingWhitespaces()
  if !&binary && &filetype != 'diff'
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
  endif
endfun
autocmd BufWritePre,FileWritePre,FileAppendPre,FilterWritePre *
  \ :call <SID>StripTrailingWhitespaces()