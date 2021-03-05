# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=500
SAVEHIST=2000
setopt autocd beep extendedglob nomatch
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/sahel/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Adding aliases
if [ -f ~/.shell_aliases ]; then
	. ~/.shell_aliases
fi
