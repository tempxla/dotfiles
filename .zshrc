# Set up the prompt

# autoload -Uz promptinit
# promptinit
# prompt fade

autoload -U colors && colors

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '{%s:%b}'

#RPROMPT=\$vcs_info_msg_0_
PROMPT="%(?..%{$fg_bold[red]%}→ %?
)%{$fg_bold[blue]%}[%n@%M] %{$fg_bold[white]%}:%l %{$fg_bold[green]%}%~ %{$fg_bold[yellow]%}\$vcs_info_msg_0_
%{$reset_color%}%# "

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
# zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
# zstyle ':completion:*' menu select=2
eval "$(dircolors ~/.dir_colors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
# zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# History search
[[ -n "${key[Up]}"   ]]  && bindkey  "${key[Up]}"    history-search-backward
[[ -n "${key[Down]}" ]]  && bindkey  "${key[Down]}"  history-search-forward

# Alias
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias l='ls -l --group-directories-first --time-style="+%Y/%m/%d %H:%M:%S"'
alias la='ls -a --group-directories-first'
alias ll='ls -al --group-directories-first --time-style="+%Y/%m/%d %H:%M:%S"'
alias lo='ls -ao --group-directories-first --time-style="+%Y/%m/%d %H:%M:%S"'
alias x='exit'
alias d='dirs -v'
alias c='clear'
alias po='popd'

alias E='emacs -nw'
alias e='emacsclient -nw'
alias e-kill="emacsclient -e '(kill-emacs)'"
alias e-daemon='emacs --daemon'
alias e-debug-init='emacs -nw --debug-init'

#alias mlterm-daemon="mlterm --daemon=genuine -e ':'"
#alias mlterm-kill="mlclient --kill"

alias g='git'
alias ga='git add'
alias gd='git diff'
alias gdw='git diff --word-diff'
alias gs='git status'
alias gp='git push'
alias gb='git branch'
alias gco='git checkout'
alias gf='git fetch'
alias gc='git commit'

alias feh-scale='feh --scale-down --auto-zoom --geometry 800x600+320+150'

#alias tweet-nowplaying='mpc current | sed -e "s/\(.*\)/#nowplaying \0/" | xargs -0 twitter-friend-list tweet'
#alias covid19='curl "https://corona-stats.online/Japan"'

# disable r
disable r

# Directory Stack
DIRSTACKFILE=~/.zsh_dirs
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
    print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20
setopt autopushd pushdsilent pushdtohome
## Remove duplicate entries
setopt pushdignoredups
## This reverts the +/- operators.
setopt pushdminus

# Syntax Highlight
if [ -f ~/github/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
source ~/github/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# cd equals pushd. list dirs `cd -<tab>`
setopt autopushd
setopt pushd_ignore_dups
