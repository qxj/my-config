# -*- mode: sh -*-
#
# http://www.gnu.org/software/bash/manual/bashref.html
#
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
# [ -z "$PS1" ] && return

# don't put duplicate lines in the history.
HISTCONTROL=ignoredups:ignorespace:erasedups
HISTSIZE=10000
HISTFILESIZE=20000

# Make some commands not show up in history
HISTIGNORE="?:??:cd -:top:pwd:exit:date:* --help"

# Don’t clear the screen after quitting a manual page
MANPAGER="less -X"

# Don't grep svn base
GREP_OPTIONS="--color --exclude=\*.svn-base"

# Enable ls output colorful
CLICOLOR=true

# Disable flow control (C-s/C-q)
stty -ixon

# Similar to zsh
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# append to the history file, don't overwrite it
shopt -s histappend
# won't blindly execute history commands, verify it firstly.
shopt -s histverify

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Enabel glob-operator ** to be recursive
[ ${BASH_VERSION%%.*} -gt 3 ] && shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Reset
ColorOff='\e[0m'       # Text Reset
# 0.Black 1.Red 2.Green 3.Yellow 4.Blue 5.Purple 6.Cyan 7.White
Colors=('\e[0;30m' '\e[0;31m' '\e[0;32m' '\e[0;33m' '\e[0;34m' '\e[0;35m' '\e[0;36m' '\e[0;37m')
BoldColors=('\e[1;30m' '\e[1;31m' '\e[1;32m' '\e[1;33m' '\e[1;34m' '\e[1;35m' '\e[1;36m' '\e[1;37m')
UnderlineColors=('\e[4;30m' '\e[4;31m' '\e[4;32m' '\e[4;33m' '\e[4;34m' '\e[4;35m' '\e[4;36m' '\e[4;37m')
BgColors=('\e[40m' '\e[41m' '\e[42m' '\e[43m' '\e[44m' '\e[45m' '\e[46m' '\e[47m')

# make prompt more friendly
function my_prompt()
{
    local last_cmd=$?

    local p_user=${Colors[1]}"\u"$ColorOff
    local p_host=${Colors[2]}"\h"$ColorOff
    local p_path=${Colors[4]}"\w"$ColorOff
    local p_time=${Colors[3]}"\t"$ColorOff
    local p_flag="$"

    # local p_last=$Green'\342\234\223'$Color_Off
    local p_last=
    if [ $last_cmd -ne 0 ]; then
        p_last=${BgColors[1]}'\342\234\227'$ColorOff
    fi
    if [ $(/usr/bin/id -u) -eq 0 ]; then
        p_user=""
        p_host=${Colors[1]}"\h"$ColorOff
        p_flag="#"
    fi
    if [ -n "$SSH_CLIENT" ]; then
        p_host=${UnderlineColors[2]}"\h"$ColorOff
    fi
    local p_git=
    if [ $(which git 2>/dev/null) ]; then
        p_git=$(__git_ps1 ${Colors[5]}"(%s) "$ColorOff 2>/dev/null)
        if [ $? -ne 0 ]; then
            p_git=${Colors[5]}'`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'$ColorOff
        fi
    fi
    PS1=$p_last$p_user"@"$p_host"["$p_time"]:"$p_git$p_path"\n"$p_flag" "
}
PROMPT_COMMAND=my_prompt

# Keep the same history in all sessions
# PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;} history -a; history -c; history -r;"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias urldecode="python -c \"import re,sys;print re.sub(r'%([0-9a-hA-H]{2})',lambda m: chr(int(m.group(1),16)), open(sys.argv[1]).read() if len(sys.argv) > 1 else sys.stdin.read())\""

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
case $OSTYPE in
    darwin*)
        test -f $(brew --prefix)/etc/bash_completion && . $(brew --prefix)/etc/bash_completion
        ;;
    linux*)
        test -f /etc/bash_completion && . /etc/bash_completion
        test -f /usr/share/autojump/autojump.sh && . /usr/share/autojump/autojump.sh
        ;;
esac

# Python virtualenvwrapper
if [ -f /usr/local/bin/virtualenvwrapper_lazy.sh ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    source /usr/local/bin/virtualenvwrapper_lazy.sh
fi

# Activiate fasd
if [ $(which fasd 2>/dev/null) ]; then
    eval "$(fasd --init auto)"
fi

# Env variables
test -d $HOME/bin && export PATH=$PATH:$HOME/bin
test -d $HOME/local/bin && export PATH=$PATH:$HOME/local/bin
test -d $HOME/.local/bin && export PATH=$PATH:$HOME/.local/bin
test -d $HOME/.cask/bin && export PATH=$PATH:$HOME/.cask/bin
export PAGER=less
export LESS="-XR"               # interpret "raw" control sequences (ipython)
export EDITOR=vim

# Mapping Caps Lock to Control
# setxkbmap -layout us -option ctrl:nocaps

test -f ~/.bash_alias && . ~/.bash_alias
test -f ~/.bash_local && . ~/.bash_local
