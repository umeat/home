PATH=/home/brandon/bin:$PATH

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
set_prompt() {
    export PS1="\[\033]2;\h:\u:\w\007\]\n\[\033[1;32m\][\u@\h:\w]$(parse_git_branch)$\[\033[0m\] "
}
PROMPT_COMMAND=set_prompt
