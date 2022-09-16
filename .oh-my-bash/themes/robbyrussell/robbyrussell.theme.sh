#! bash oh-my-bash.module

function _omb_theme_PROMPT_COMMAND() {
    # color options: see lib/omb-prompt-colors.sh

    if [[ "$?" == 0 ]]; then
        local arrow_color="${_omb_prompt_bold_green}"
    else
        local arrow_color="${_omb_prompt_bold_brown}"
    fi

    # local user_name="${_omb_prompt_white}\u${_omb_prompt_reset_color}"
    local base_directory="${_omb_prompt_bold_teal}\W${_omb_prompt_reset_color}${_omb_prompt_normal}"
    local SCM_THEME_PROMPT_PREFIX="${_omb_prompt_bold_navy}git:(${_omb_prompt_bold_brown}"
    local SCM_THEME_PROMPT_SUFFIX="${_omb_prompt_reset_color}"
    local SCM_THEME_PROMPT_CLEAN="${_omb_prompt_bold_navy})${_omb_prompt_reset_color}"
    local SCM_THEME_PROMPT_DIRTY="${_omb_prompt_bold_navy}) ${_omb_prompt_olive}✗${_omb_prompt_reset_color}"

    local arrow="${arrow_color}➜${_omb_prompt_reset_color}"

    # PS1="${arrow}  ${user_name} ${base_directory} "
    PS1="${arrow}  ${base_directory} "

    local scm_info=$(scm_prompt_info)

    PS1+=${scm_info:+$scm_info }
}

_omb_util_add_prompt_command _omb_theme_PROMPT_COMMAND
