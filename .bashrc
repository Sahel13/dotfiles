#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

# Adding aliases
if [ -f ~/.shell_aliases ]; then
	. ~/.shell_aliases
fi
