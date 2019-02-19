# Set up the prompt

autoload -Uz promptinit
promptinit
#prompt adam1

setopt histignorealldups sharehistory

# Use vi keybindings
bindkey -v
export KEYTIMEOUT=1

# Indicate vi mode with cursor
function zle-keymap-select zle-line-init
{
	case $KEYMAP in
		vicmd) echo -ne "\e[1 q";;
		viins|main) echo -ne "\e[5 q";;
	esac

	zle reset-prompt
	zle -R
}

zle -N zle-line-init
zle -N zle-keymap-select

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

#########
# zplug #
#########

source ~/.zplug/init.zsh


# Pretty, minimal and fast ZSH prompt
zplug "mafredri/zsh-async", from:github
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme

# Fish shell like syntax highlighting for Zsh
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# z - jump around
zplug "rupa/z", use:z.sh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Source plugins and add commands to $PATH
zplug load --verbose


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

path+=$HOME/bin
