stty werase undef
bind '\C-w:unix-filename-rubout'

export PATH=$HOME/.local/bin:$HOME/go/bin:$PATH

alias calc=qalculate-gtk
alias vi=vim
alias ssh='TERM=xterm ssh -o ServerAliveInterval=60 -o SendEnv=TERM'
alias please='sudo $(fc -ln -1)'

export EDITOR=vim

source ~/.config/git-completion.bash

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
set_prompt() {
    export PS1="\[\033]2;\h:\u:\w\007\]\n\[\033[1;32m\][\u@\h:\w]$(parse_git_branch)$\[\033[0m\] "
}
PROMPT_COMMAND=set_prompt

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export HISTCONTROL=ignoredups:erasedups

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
