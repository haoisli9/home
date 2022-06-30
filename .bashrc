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

alias vim='nvim'

# no case sensitive when autocomplete
shopt -s nocaseglob

# do not uncomment it.
# PS1="[\[\e[32;40m\]\u@\h \[\e[33;40m\]\W\e[m] \$ "

. ~/z.sh

export LANG=zh_CN.UTF-8
# use zsh
# bash -c zsh

# If not running interactively, do not do anything
# [[ $- != *i* ]] && return
# [[ -z "$TMUX" ]] && exec tmux

