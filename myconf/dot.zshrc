# -*- mode: sh -*-
#
# ~/.zshrc, zsh configurations
#

bindkey -e                      # Use emacs key bindings

################
# Options
################
zstyle ':completion:*' menu select
zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

# stop backward-kill-word on directory delimiter
# WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
autoload -U select-word-style
select-word-style bash

autoload -U compinit && compinit
zmodload -i zsh/complist

unsetopt menu_complete
unsetopt flow_control

# History options
HISTFILE=$HOME/.zsh_history
HISTSIZE=99
SAVEHIST=10000
HISTORY_IGNORE="(bg|fg|rm*|clear|ls|pwd|history|exit|* --help)"
setopt always_to_end
setopt append_history
setopt auto_menu
setopt complete_in_word
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_fcntl_lock
setopt hist_verify
setopt inc_append_history
setopt interactive_comments
setopt share_history

################
# Zplug
################
[[ -d ~/.zplug ]] || {
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
}

source ~/.zplug/init.zsh

#zplug "zsh-users/zsh-autosuggestions", defer:2, use:"*.zsh", \
#  hook-load: "ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=cyan,bold' \
#              ZSH_AUTOSUGGEST_USE_ASYNC=true"

# zplug "olivierverdier/zsh-git-prompt", use:zshrc.sh
# zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf, use:"*darwin*amd64*"
zplug "plugins/git", from:oh-my-zsh, if:"which git"
zplug "plugins/osx", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"
zplug "mafredri/zsh-async", defer:0
zplug "sindresorhus/pure", use:pure.zsh, as:theme
#zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme
POWERLEVEL9K_MODE="nerdfont-fontconfig"
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=4
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon root_indicator dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs time virtualenv)
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
POWERLEVEL9K_MULTILINE_SECOND_PROMPT_PREFIX=" ❯ "

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "zsh-users/zsh-completions"
#zplug "Vifon/deer", use:deer
zplug "supercrabtree/k"

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

# https://github.com/joshtronic/dotfiles/blob/master/zshrc

################
# User configuration
################
PATH=$HOME/.local/bin:$PATH
PATH=$PATH:/usr/local/bin
PATH=$PATH:/usr/local/sbin
PATH=$PATH:/usr/local/texlive/2016basic/bin/x86_64-darwin
PATH=$PATH:~/anaconda2/bin
export PATH

export EDITOR=vim
export TERM=xterm-256color

# let gtags treat .h as c++ source file
export GTAGSFORCECPP=true

# Don’t clear the screen after quitting a manual page
export MANPAGER="less -X"

# homebrew bottles ustc mirror
export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles

if which fasd >/dev/null; then
    eval "$(fasd --init auto)"
fi

# Java environment
export JAVA_HOME=`/usr/libexec/java_home`
export JAVAFX_HOME=$JAVA_HOME/jre/lib
if which jenv >/dev/null; then
  export JENV_ROOT=/usr/local/var/jenv
  #eval "$(jenv init -)"
fi

# Python environment
if which pyenv >/dev/null; then
  export PYENV_ROOT=/usr/local/var/pyenv
  eval "$(pyenv init -)"
  if which pyenv-virtualenv-init >/dev/null; then
    eval "$(pyenv virtualenv-init -)"
  fi
fi

if [ -f /usr/local/bin/virtualenvwrapper_lazy.sh ]; then
  export WORKON_HOME=$HOME/.virtualenvs
  source /usr/local/bin/virtualenvwrapper_lazy.sh
fi

# Support fzf
if [ -f ~/.fzf.zsh ]; then
    source ~/.fzf.zsh
    export FZF_DEFAULT_OPTS='--height 40% --reverse --border'
    export FZF_DEFAULT_COMMAND='ag -l -g ""'
    export FZF_DEFAULT_OPTS='--multi'
    bindkey '^R' fzf-history-widget
    bindkey '^X^T' fzf-file-widget
    bindkey '^X^Q' fzf-cd-widget
    # Restore CTRL-T, ALT-C
    bindkey '^T' transpose-chars
    bindkey '^[c' capitalize-word
fi

# Pyspark and ipython notebook integration
# https://gist.github.com/ololobus/4c221a0891775eaa86b0
if which pyspark >/dev/null; then
  export SPARK_HOME="/usr/local/Cellar/apache-spark/2.1.0/libexec/"
  export PYTHONPATH=$SPARK_HOME/python:$SPARK_HOME/python/build:$PYTHONPATH
  export PYTHONPATH=$SPARK_HOME/python/lib/py4j-0.10.4-src.zip:$PYTHONPATH
  export PYSPARK_DRIVER_PYTHON=`which ipython`
fi

################
# Aliases
################
alias v='f -e vim'
alias o='a -e open'

alias ls='ls -G'
alias l='k --no-vcs'

GREP_EXCLUDE_DIR="{.git,vendor}"
GREP_FLAGS=" --color=auto --exclude-dir=${GREP_EXCLUDE_DIR}"
alias grep="grep ${GREP_FLAGS}"
alias egrep="egrep ${GREP_FLAGS}"
alias fgrep="fgrep ${GREP_FLAGS}"

################
# Keybinds
################

# Search history use up/down arrow keys.
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
bindkey "^U"   backward-kill-line # instead of kill-whole-line
bindkey ' '    magic-space        # also do history expansion on space
bindkey '\ei'  menu-complete      # menu completion via esc-i
bindkey -s '\eo' 'cd ..\n'        # goto parent directory
# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

################
# Functions
################
man() {
  env \
    LESS_TERMCAP_md=$'\e[1;36m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[1;40;92m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[1;32m' \
    man "$@"
}

ssh() {
  if [ -z ${TMUX+x} ]; then
    command ssh "$@"
  else
    tmux rename-window "$*"
    command ssh "$@"
    tmux set-window-option automatic-rename "on" 1>/dev/null
  fi
}

# go to a path relative to the top directory of the current git worktree.
cdg() {
  local topdir=$(git rev-parse --show-toplevel)
  if [[ $? -ne 0 ]]; then
    return 1
  fi
  cd "${topdir}/${1}"
}

################
# Local zsh configuration
################
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
