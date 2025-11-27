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
zstyle ':completion::complete:*' use-cache 1
zstyle ':conda_zsh_completion:*' use-groups true

# stop backward-kill-word on directory delimiter
# WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
autoload -U select-word-style
select-word-style bash

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
# Zinit   https://www.jianshu.com/p/2e098dfecf4a
################
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local}/zinit/zinit.git"
[[ -d $ZINIT_HOME ]] || {
  mkdir -p "$(dirname $ZINIT_HOME)" && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
}
source "${ZINIT_HOME}/zinit.zsh"

zinit ice compile'(pure|async).zsh' pick"async.zsh" src"pure.zsh"; zinit light sindresorhus/pure
zinit wait silent for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" zdharma-continuum/fast-syntax-highlighting \
  blockf zsh-users/zsh-completions \
  atload"!_zsh_autosuggest_start" zsh-users/zsh-autosuggestions \
  if'(( $+commands[git] ))' OMZP::git \
  if'[[ $OSTYPE == *darwin* ]]' OMZP::macos
#zinit ice from"gh-r" as"program"; zinit load junegunn/fzf-bin

# enable zsh autocomplete
autoload -U compinit && compinit
zmodload -i zsh/complist

# 在 Pure 的 preprompt 里追加“当前时间”段

################
# User configuration
################
RPROMPT='%F{245}[%*]%f'

PATH=$HOME/.local/bin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/usr/local/sbin:$PATH
export PATH

export EDITOR=vim
export TERM=xterm-256color

# let gtags treat .h as c++ source file
export GTAGSFORCECPP=true

# Don’t clear the screen after quitting a manual page
export MANPAGER="less -X"

# homebrew tsinghua mirror
export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git"
export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git"
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"
export HOMEBREW_API_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api"
export HOMEBREW_NO_ENV_HINTS="1"

export PS_FORMAT="pid,ppid,user,pri,ni,vsz,rss,pcpu,pmem,tty,stat,args"

export FD_OPTIONS="--follow --exclude .git --exclude node_modules"
export BAT_PAGER="less -R"

# Java environment
if [[ $OSTYPE == *darwin* ]]; then
  export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
  export JAVAFX_HOME=$JAVA_HOME/jre/lib
fi
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

#if [ -f $HOME/miniconda3/bin/python ]; then
#  export PATH=$HOME/miniconda3/bin:$PATH
#fi

# quick access to files and directories
if which fasd >/dev/null; then
    eval "$(fasd --init auto)"
    bindkey '^X^A' fasd-complete
fi

[ -f /opt/homebrew/etc/profile.d/autojump.sh ] && . /opt/homebrew/etc/profile.d/autojump.sh

# command-line fuzzy finder
if [ -f ~/.fzf.zsh ]; then
    source ~/.fzf.zsh
    export FZF_DEFAULT_OPTS="--height 40% --reverse --border --no-mouse -1 --multi --inline-info --preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {} 2>/dev/null | head -300' --preview-window='right:hidden:wrap' --bind='f3:execute(bat --style=numbers {} || less -f {}),f2:toggle-preview,ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-a:select-all+accept,ctrl-y:execute-silent(echo {+} | pbcopy)'"
    # Use `git ls-files` inside git repo, otherwise ..
    #export FZF_DEFAULT_COMMAND="git ls-files --cached --others --exclude-standard | ag -l -g ''"
    export FZF_DEFAULT_COMMAND="git ls-files --cached --others --exclude-standard | fd --type f --type l $FD_OPTIONS"
    export FZF_CTRL_T_COMMAND="fd $FD_OPTIONS"
    export FZF_ALT_C_COMMAND="fd --type d $FD_OPTIONS"
    bindkey '^R' fzf-history-widget
    bindkey '^X^T' fzf-file-widget # CTRL-T
    bindkey '^X^Q' fzf-cd-widget   # ALT-C
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

[[ $OSTYPE == *darwin* ]] && alias ls='ls -G' || alias ls='ls --color'
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
bindkey "^[[A" history-search-backward # <Up>
bindkey "^[[B" history-search-forward  # <Down>
bindkey "^U"   backward-kill-line      # instead of kill-whole-line
bindkey ' '    magic-space             # also do history expansion on space
bindkey '\ei'  menu-complete           # <ESC>i, menu completion
bindkey -s '\eo' 'cd ..\n'             # <ESC>o, goto parent directory
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


