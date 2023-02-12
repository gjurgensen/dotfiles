# Stuff related to version control: git, svn

function get_vc_ignores {
  builtin cd $1
  # Check if in a git repository
  if git status &>/dev/null; then
    ls --almost-all | git check-ignore --stdin --no-index | tr '\n' ' '
  # Check if in a svn repository
  elif svn info &>/dev/null; then
    svn propget svn:ignore | tr '\n' ' '
  fi
}

function lv {
  # This is very fragile, only works on simple cases
  target_dir=$(ls -d $@)

  local ls_ignore=$(get_vc_ignores $target_dir)
  li "$@"
}

function liv {
  # This is very fragile, only works on simple cases
  target_dir=$(ls -d $@)

  local ls_ignore="$ls_ignore $(get_vc_ignores $target_dir)"
  li "$@"
}


# TODO: svn diff to temp file, and then only view if svn has good error code
# Also maybe don't view empty diff
function svndiff {
  # svn diff $@ --git | view
  svn diff $@ --git | view
}

function svnlog {
  svn log -l 20 | view
}
