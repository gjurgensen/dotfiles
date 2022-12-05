mkcd(){
  mkdir $1
  cd $1
}

bak(){
    cp -r $1 $1.bak
}

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

alias emacs='TERM=xterm-emacs emacsclient --alternate-editor="" -nw'
alias gemacs='emacsclient -c -n --alternate-editor=""'
alias killemacs='emacsclient -e "(kill-emacs)"'

alias lsa='ls -I "*.acl2" -I "*.cert" -I "*.cert.out" -I "*.lx64fsl" -I "*.lx86cl64" -I "*.port"'

alias lsi='ls -I "*.acl2" -I "*.cert" -I "*.cert.out" -I "*.lx64fsl" -I "*.lx86cl64" -I "*.port" -I "*.o" -I "*.bak*" -I "*~"'

alias view='vim - -R --not-a-term'

alias open='xdg-open'

alias sup='sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y'

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
  svn diff $@ --git | view
}

export EDITOR=nvim
export MANPAGER='nvim +Man!'

export TERM=xterm-256color

rc_local="${HOME}/.rc_local.sh"
if [ -f "${rc_local}" ]; then
  source "${rc_local}"
else
  echo "# Add machine-specific configuration here\n\n# export TERM_COLOR=\"#000000\"" > ${rc_local}
fi

if [ -n "${TERM_COLOR}" ]; then
  printf "\e]11;${TERM_COLOR}\e\\";
fi

# https://unix.stackexchange.com/questions/315395/how-to-automatically-run-a-command-after-exiting-ssh
ssh(){
  command ssh "$@";
  if [ -z "${TERM_COLOR}" ]; then
    printf "\e]111;\e\\";
  else
    printf "\e]11;${TERM_COLOR}\e\\";
  fi
}

if [[ -v TMUX_ON_START && -z "$TMUX" ]]; then
  ([[ -v TMUX_ATTACH_ON_START ]] && tmux attach) || tmux new-session
fi
