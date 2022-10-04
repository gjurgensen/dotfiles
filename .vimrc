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

" line numbering
set number relativenumber
highlight LineNr ctermfg=grey

" https://stackoverflow.com/questions/356126/how-can-you-automatically-remove-trailing-whitespace-in-vimutocmd BufWritePre :%s/\s\+$//e

function! <SID>StripTrailingWhitespaces()
  if !&binary && &filetype != 'diff'
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
  endif
endfun

autocmd BufWritePre,FileWritePre,FileAppendPre,FilterWritePre *
  \ :call <SID>StripTrailingWhitespaces()
