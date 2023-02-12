function term_color {
  if [ -z "${TERM_COLOR}" ]; then
    printf "\e]111;\e\\";
  else
    printf "\e]11;${TERM_COLOR}\e\\";
  fi
}

function set_term_color {
  export TERM_COLOR="$1";
  term_color
}

# Some systems (notably WSL) have an unreadable default for ls directory color.
function fix_ls_colors {
    export LS_COLORS="${LS_COLORS}:ow=01;38"
}

function set_tmux_name {
  if [ -n "$TMUX" ]; then
    echo "$(tmux display-message -p '#W')";
    tmux rename-window "$1"
  fi
}

# $1: prompt
# $2: (optional) default answer
# TODO: optionally pass name of env var to use instead of "yesno"?
function prompt_yesno {
  local default=false
  if [ -n "$2" ]; then
    if [ "$2" = "y" ] || [ "$2" = "Y" ]; then
      default="y"
    elif [ "$2" = "n" ] || [ "$2" = "N" ]; then
      default="n"
    fi
  fi

  local prompt_suffix=""
  if [ "$default" = "y" ]; then
    prompt_suffix="[Y/n]"
  elif [ "$default" = "n" ]; then
    prompt_suffix="[y/N]"
  else
    prompt_suffix="[y/n]"
  fi

  while true; do
    printf "$1 $prompt_suffix "
    # TODO: is this portable to bash?
    read -sk yesno

    # Can't really handle unprintables in strings, so we use sed to "normalize"
    local yesno_newline=$(printf "$yesno" | sed -n l)
    if [ "$yesno_newline" = "$" ]; then
      export yesno="$default"
      echo "$yesno"
      return
    fi
    echo "$yesno"

    if [ "$yesno" = "y" ] || [ "$yesno" = "Y" ]; then
      export yesno="y"
      return
    elif [ "$yesno" = "n" ] || [ "$yesno" = "N" ]; then
      export yesno="n"
      return
    fi
  done
}
