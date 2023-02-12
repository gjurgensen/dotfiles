################################################################################

function gen_ed_shell {
  alias more='cat'
  alias less='cat'
  alias view='cat'
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

alias e='$EDITOR'


################################################################################

## Emacs

alias emacs='TERM=xterm-emacs emacsclient --alternate-editor="" -nw'
alias gemacs='emacsclient -c -n --alternate-editor="" $@'

alias kill-emacs='emacsclient -e "(kill-emacs)"'

function _emacs_shell_cmd {
  local CMD
  if [[ -n "$1" ]]; then
    echo "\"cd $(realpath $1)\""
  fi
}

function _emacs_shell {
  TERM=xterm-emacs emacsclient --alternate-editor="" -nw -e "(fresh-shell $(_emacs_shell_cmd $1))"
}
alias emacs-shell='_emacs_shell'

function _gemacs_shell {
  emacsclient -c -n --alternate-editor="" -e "(fresh-shell $(_emacs_shell_cmd $1))"
}
alias gemacs-shell='_gemacs_shell'

function unemacs {
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


# Alternative commands if in an emacs shell
if [[ -n "$EMACS_SERVER" ]]; then
  function _emacs_shell {
    emacsclient --socket-name=$EMACS_SERVER -e "(fresh-shell $(_emacs_shell_cmd $1))"
  }
  unalias gemacs-shell
  alias gemacs-shell='emacs-shell'
  unalias emacs
  function emacs {
    emacsclient -e "(find-file \"$(realpath $1)\")" -s "$EMACS_SERVER" > /dev/null
  }
  unalias gemacs
  alias gemacs='emacs'
  unalias e
  alias e='emacs'
  function man {
    emacsclient -e "(man \"$1\")" -s "$EMACS_SERVER" > /dev/null
  }
  function setd {
    emacsclient -e "(cd \"$(realpath $1)\")" -s "$EMACS_SERVER" > /dev/null
  }
  gen_ed_shell
fi


################################################################################

# (n)vim

export VIMINIT="source ~/.config/nvim/init.vim"

# command vim - -R --not-a-term $@
function view {
  local temp=$(mktemp)
  cat > $temp
  nvim -R $temp $@
  rm $temp
}

if [[ -n "$NVIM" ]]; then
  function nvim {
    command nvim --server "$NVIM" --remote-send "<ESC>:e $(realpath $1)<CR>"
  }
  function man {
    command nvim --server "$NVIM" --remote-send "<ESC>:Man $1<CR>"
  }
  function setd {
    command nvim --server "$NVIM" --remote-send "<ESC>:cd $(realpath $1)<CR>i"
  }
  alias e='nvim'
  gen_ed_shell
fi
