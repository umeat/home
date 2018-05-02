PATH=/home/brandon/bin:$PATH

alias calc=qalculate-gtk

# https://github.com/git/git/blob/master/contrib/completion/git-completion.bash
source ~/.config/git-completion.bash

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
set_prompt() {
    export PS1="\[\033]2;\h:\u:\w\007\]\n\[\033[1;32m\][\u@\h:\w]$(parse_git_branch)$\[\033[0m\] "
}
PROMPT_COMMAND=set_prompt
