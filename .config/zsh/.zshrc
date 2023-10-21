# Lines configured by zsh-newuser-install
HISTFILE=$ZDOTDIR/.histfile
HISTSIZE=500
SAVEHIST=2000
setopt autocd beep extendedglob nomatch
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '$ZDOTDIR/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Customize the prompt
PROMPT="%2~ $ "

# Adding aliases
if [ -f ~/.shell_aliases ]; then
	. ~/.shell_aliases
fi

# Vi mode
bindkey -v

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/sahel/Misc/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/sahel/Misc/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/sahel/Misc/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/sahel/Misc/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

source $ZDOTDIR/extensions/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZDOTDIR/extensions/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
