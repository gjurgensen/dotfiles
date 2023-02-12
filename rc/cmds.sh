function mkcd {
  mkdir $1
  cd $1
}

function bak {
  cp -r $1 $1.bak
}

function unbak {
  if [ ! -f "$1.bak" ]; then
    echo "File $1.bak does not exist"
    return
  fi

  if [ -f "$1" ]; then
    prompt_yesno "Overwrite file $1?" "n"
    [ "$yesno" = "n" ] && return
  fi

  mv $1.bak $1
}

function whereami {
  echo $HOST
}

# https://www.marcinkossakowski.com/posts/2019-06-13-how-to-preview-csv-files-in-terminal.html
function viewcsv {
  cat $1 | sed -e 's/,/ ,/g' | column -t -s, | less -S
}

function latexc {
  if [ -z "$(which pdflatex)" ]; then
    echo "pdflatex not found"
  elif [ -z "$(which bibtex)" ]; then
    echo "bibtex not found"
  else
    target=${1%.tex}
    pdflatex $target && bibtex $target && pdflatex $target && pdflatex $target
  fi
}

# Likely shadows bin `open`, alias for `xdg-open`
function open {
  local ARG
  if [[ -z "$1" ]]; then
    ARG="."
  else
    ARG="$1"
  fi
  xdg-open "$ARG"
}
function sopen {
  open "$@" > /dev/null 2> /dev/null
}


alias sup='sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y'

export ls_ignore="*.o *.bak *~"
function li {
  local ignores=$(printf "$ls_ignore" | sed -E "s/([^[:blank:]]+)/-I \"\1\"/g")
  eval "ls $ignores $@"
}

alias grepr='grep --color=always -HR'

# https://askubuntu.com/questions/359492/create-a-shortcut-for-url
function url_shortcut {
  if [[ -z "$1" ]]; then
    echo "Expected url argument" >&2
  else
    echo "<meta http-equiv=\"refresh\" content=\"0;url=$1\">"
  fi
}

# https://unix.stackexchange.com/questions/315395/how-to-automatically-run-a-command-after-exiting-ssh
# Depends on utils
function ssh {
  command ssh "$@";
  term_color
}
