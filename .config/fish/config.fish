alias la='ls -a'
alias ll='ls -al'
alias x='exit'
alias e='emacs -nw'
alias sudoe='sudo emacs -nw'
alias tweet-nowplaying='mpc current | sed -e "s/\(.*\)/#nowplaying \0/" | xargs -0 twitter-friend-list tweet'

# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

# Status Chars
set __fish_git_prompt_char_dirtystate '★'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '☆'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'
