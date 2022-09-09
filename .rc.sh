whereami(){
  echo $HOST
}

# https://www.marcinkossakowski.com/posts/2019-06-13-how-to-preview-csv-files-in-terminal.html
viewcsv(){
  cat $1 | sed -e 's/,/ ,/g' | column -t -s, | less -S
}

latexc(){
  if [ -z "$(which pdflatex)" ]; then
    echo "pdflatex not found"
  elif [ -z "$(which bibtex)" ]; then
    echo "bibtex not found"
  else
    target=${1%.tex}
    pdflatex $target && bibtex $target && pdflatex $target && pdflatex $target
  fi
}

alias emacs='emacs -nw'

alias lsa='ls -I "*.acl2" -I "*.cert" -I "*.cert.out" -I "*.lx64fsl" -I "*.port"'

# TODO:
# - how to point git-check-ignore to the right directory?
# - add svn support
li(){
  ignore=$(ls $1 | git check-ignore --stdin --no-index)

  ignore_args=""
  IFS_BAK=${IFS}
  IFS=$" "
  for i in ${ignore};
    do ignore_args="${ignore_args} -I $i"
  done
  # Is this necessary? Scope is unclear here
  IFS=${IFS_BAK}

  #echo "ls${ignore_args} $1"
  ls${ignore_args} $1
}

# TODO: svn diff to temp file, and then only view if svn has good error code
# Also maybe don't view empty diff
svndiff(){
  svn diff $@ --git | view - --not-a-term
}

export TERM=xterm-256color

rc_local="${HOME}/.rc_local.sh"
if [ -f "${rc_local}" ]; then
  source "${rc_local}"
else
  echo "# Add machine-specific configuration here" > ${rc_local}
fi

# https://unix.stackexchange.com/questions/480451/how-to-start-tmux-upon-running-terminal
# get into tmux at start
if [[ -v TMUX_ON_START && -z "$TMUX" ]]; then
  tmux attach || tmux new-session
fi
