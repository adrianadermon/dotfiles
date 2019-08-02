unsetopt BG_NICE

alias exa=exa-linux-x86_64

export EDITOR='nvim'

export FZF_DEFAULT_COMMAND='fd --type f --color=never'
export FZF_DEFAULT_OPTS='--preview "bat --color always --line-range :25 --plain {}"'

typeset -U path
path=(~/bin $path)
