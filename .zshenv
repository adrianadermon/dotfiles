unsetopt BG_NICE

alias exa=exa-linux-x86_64

export FZF_DEFAULT_COMMAND='fd --type f --color=never'
export FZF_DEFAULT_OPTS='--preview "bat --color always --line-range :25 --plain {}"'
