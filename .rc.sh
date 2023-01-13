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

term_color() {
  if [ -z "${TERM_COLOR}" ]; then
    printf "\e]111;\e\\";
  else
    printf "\e]11;${TERM_COLOR}\e\\";
  fi
}

set_term_color() {
  export TERM_COLOR="$1";
  term_color
}

unemacs(){
  local ARG
  if [[ -z "$1" ]]; then
    ARG="."
  else
    ARG="$1"
  fi

  rm -if $ARG/*~
  rm -if $ARG/*#*#*
  # rm -if .#*
}

alias kill-emacs='emacsclient -e "(kill-emacs)"'

_emacs_shell() {
  local CMD
  if [[ -n "$1" ]]; then
    CMD="\"cd $(realpath $1)\""
  fi

  if [[ -n "$EMACS_SERVER" ]]; then
    # emacsclient --socket-name=$EMACS_SERVER -e "(switch-to-buffer (new-shell-get-buffer-create))"
    emacsclient --socket-name=$EMACS_SERVER -e "(new-shell-cmd $CMD)"
  else
    # TERM=xterm-emacs emacsclient --alternate-editor="" -nw -e "(switch-to-buffer (new-shell-get-buffer-create))"
    TERM=xterm-emacs emacsclient --alternate-editor="" -nw -e "(new-shell-cmd $CMD)"
  fi
}
alias emacs-shell='_emacs_shell'

_gemacs_shell() {
  local CMD
  if [[ -n "$1" ]]; then
    CMD="\"cd $(realpath $1)\""
  fi

  if [[ -n "$EMACS_SERVER" ]]; then
    # emacsclient --socket-name=$EMACS_SERVER -n --alternate-editor="" -e "(switch-to-buffer (new-shell-get-buffer-create))"
    emacsclient --socket-name=$EMACS_SERVER -n --alternate-editor="" -e "(new-shell-cmd $CMD)"
  else
    # emacsclient -c -n --alternate-editor="" -e "(switch-to-buffer (new-shell-get-buffer-create))"
    emacsclient -c -n --alternate-editor="" -e "(new-shell-cmd $CMD)"
  fi
}
alias gemacs-shell='_gemacs_shell'

emacs() {
  if [[ -n "$EMACS_SERVER" ]]; then
    emacsclient -e "(find-file \"$(realpath $1)\")" -s "$EMACS_SERVER" > /dev/null
  else
    TERM=xterm-emacs emacsclient --alternate-editor="" -nw $@
  fi
}

gemacs() {
  if [[ -n "$EMACS_SERVER" ]]; then
    emacsclient -e "(find-file \"$(realpath $1)\")" -s "$EMACS_SERVER" > /dev/null
  else
    emacsclient -c -n --alternate-editor="" $@
  fi
}

nvim() {
  if [[ -n "$NVIM" ]]; then
    command nvim --server "$NVIM" --remote-send "<ESC>:e $(realpath $1)<CR>"
  else
    command nvim $@
  fi
}

if [ -n "$(which nvim)" ]; then
  export EDITOR=nvim;
  export MANPAGER='nvim +Man!';
elif [ -n "$(which vim)" ]; then
  export EDITOR=vim;
  export MANPAGER='vim -M +MANPAGER -';
elif [-n "$(which emacs)" ]; then
  export EDITOR=emacs
fi

man() {
  if [[ -n "$NVIM" ]]; then
    command nvim --server "$NVIM" --remote-send "<ESC>:Man $1<CR>"
  elif [[ -n "$EMACS_SERVER" ]]; then
    emacsclient -e "(man \"$1\")" -s "$EMACS_SERVER" > /dev/null
  else
    command man $@
  fi
}

e() {
  if [[ -n "$NVIM" ]]; then
    command nvim --server "$NVIM" --remote-send "<ESC>:e $(realpath $1)<CR>"
  elif [[ -n "$EMACS_SERVER" ]]; then
    emacsclient -e "(find-file \"$(realpath $1)\")" -s "$EMACS_SERVER" > /dev/null
  else
    $EDITOR $@
  fi
}

less() {
  if [[ -n "$NVIM" || -n "$EMACS_SERVER" ]]; then
    cat
  else
    command less $@
  fi
}

more() {
  if [[ -n "$NVIM" || -n "$EMACS_SERVER" ]]; then
    cat
  else
    command more $@
  fi
}

# alias view='vim - -R --not-a-term'
view() {
  if [[ -n "$NVIM" || -n "$EMACS_SERVER" ]]; then
    cat
  else
    command vim - -R --not-a-term $@
  fi
}

cd() {
  if [[ -n "$NVIM" ]]; then
    command nvim --server "$NVIM" --remote-send "<ESC>:cd $(realpath $1)<CR>i"
  elif [[ -n "$EMACS_SERVER" ]]; then
    emacsclient -e "(cd \"$(realpath $1)\")" -s "$EMACS_SERVER" > /dev/null
  fi
  builtin cd $@
}

alias lsa='ls -I "*.acl2" -I "*.cert" -I "*.cert.out" -I "*.lx64fsl" -I "*.lx86cl64" -I "*.port"'

alias lsi='ls -I "*.acl2" -I "*.cert" -I "*.cert.out" -I "*.lx64fsl" -I "*.lx86cl64" -I "*.port" -I "*.o" -I "*.bak*" -I "*~"'

# Likely shadows bin `open`, alias for `xdg-open`
open(){
  local ARG
  if [[ -z "$1" ]]; then
    ARG="."
  else
    ARG="$1"
  fi
  xdg-open "$ARG"
}
sopen(){
  open "$@" > /dev/null 2> /dev/null
}

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

svnlog(){
  svn log -l 20 | view
}

alias grepr='grep --color=always -HR'

# https://askubuntu.com/questions/359492/create-a-shortcut-for-url
url_shortcut(){
  if [[ -z "$1" ]]; then
    echo "Expected url argument" >&2
  else
    echo "<meta http-equiv=\"refresh\" content=\"0;url=$1\">"
  fi
}

# Some systems (notably WSL) have an unreadable default for ls directory color.
fix_ls_colors(){
    export LS_COLORS="${LS_COLORS}:ow=01;38"
}

set_tmux_name(){
  if [ -n "$TMUX" ]; then
    echo "$(tmux display-message -p '#W')";
    tmux rename-window "$1"
  fi
}

export TERM=xterm-256color

rc_local="${HOME}/.rc_local.sh"
if [ -f "${rc_local}" ]; then
  source "${rc_local}"
else
  echo "# Add machine-specific configuration here\n\n# set_term_color \"#000000\"" > ${rc_local}
fi

# https://unix.stackexchange.com/questions/315395/how-to-automatically-run-a-command-after-exiting-ssh
ssh(){
  command ssh "$@";
  term_color
}

if [[ -v TMUX_ON_START && -z "$TMUX" ]]; then
  ([[ -v TMUX_ATTACH_ON_START ]] && tmux attach) || tmux new-session
fi
