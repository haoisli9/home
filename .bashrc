# Aliases
alias df='df -h'
alias du='du -h'
# Misc :)
alias less='less -r' # raw control characters
alias whence='type -a' # where, of a sort
# Some shortcuts for different directory listings
alias ls='ls -hF --color=tty' # classify files in colour
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'
alias ll='ls -l' # long list
alias la='ls -al' # all but . and ..
# alias l='ls -CF' \
alias cp='cp -i'
alias rm='rm -i'
# search
alias grep='grep --color' # show differences in colour
alias egrep='egrep --color=auto' # show differences in colour
alias fgrep='fgrep --color=auto' # show differences in colour

alias rld='exec bash'

# no case sensitive when autocomplete
shopt -s nocaseglob

PS1="[\[\e[32;40m\]\u@\h \[\e[33;40m\]\W\e[m] \$ "

. /d/Unix/bin/z.sh

# use zsh
# bash -c zsh

# TMUX
# if which tmux >/dev/null 2>&1; then
#     # if no session is started, start a new session
#     test -z ${TMUX} && tmux

#     # when quitting tmux, try to attach
#     while test -z ${TMUX}; do
#         tmux attach || break
#     done
# fi

