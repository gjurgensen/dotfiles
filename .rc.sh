whereami(){
  echo $HOST
}

# https://www.marcinkossakowski.com/posts/2019-06-13-how-to-preview-csv-files-in-terminal.html
viewcsv(){
  cat $1 | sed -e 's/,/ ,/g' | column -t -s, | less -S
}

alias emacs='emacs -nw'

export TERM=xterm-256color

source ~/.rc_local.sh

# https://unix.stackexchange.com/questions/480451/how-to-start-tmux-upon-running-terminal
# get into tmux at start
# if [ -z "$TMUX" ]; then
#   tmux new-session
# fi
