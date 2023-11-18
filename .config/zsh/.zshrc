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

source $ZDOTDIR/extensions/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZDOTDIR/extensions/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
