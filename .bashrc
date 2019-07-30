PATH=/home/brandon/bin:/home/brandon/.local/bin:$PATH

alias calc=qalculate-gtk
alias vi=vim

export EDITOR=vim

source ~/.config/git-completion.bash

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
set_prompt() {
    export PS1="\[\033]2;\h:\u:\w\007\]\n\[\033[1;32m\][\u@\h:\w]$(parse_git_branch)$\[\033[0m\] "
}
PROMPT_COMMAND=set_prompt

function credstash() {
    "`which credstash`.py" $@
}
export -f credstash

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
