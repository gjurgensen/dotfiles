source ~/dotfiles/rc/utils.sh
source ~/dotfiles/rc/cmds.sh
source ~/dotfiles/rc/vc.sh
source ~/dotfiles/rc/ed.sh

alias gsim='./simulate --extra-qemu-args="-S -s"'


export TERM=xterm-256color

rc_local="${HOME}/.rc_local.sh"
if [ -f "${rc_local}" ]; then
  source "${rc_local}"
else
  echo "# Add machine-specific configuration here\n\n# set_term_color \"#000000\"" > ${rc_local}
fi

if [[ -v TMUX_ON_START && -z "$TMUX" ]]; then
  ([[ -v TMUX_ATTACH_ON_START ]] && tmux attach) || tmux new-session
fi

export PATH=$PATH:$HOME/dotfiles/scripts
