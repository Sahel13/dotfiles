typeset -U path PATH
path=(
  ~/.local/bin
  ~/Misc/texlive/bin/x86_64-linux
  ~/.local/share/gem/ruby/3.0.0/bin
  $path
)
export PATH

# For consistent font sizes in alacritty across terminals. From:
# https://wiki.archlinux.org/title/Alacritty#Different_font_size_on_multiple_monitors
# https://github.com/alacritty/alacritty/issues/5101
export WINIT_X11_SCALE_FACTOR=1

# Install Ruby Gems to ~/Misc/gems
export GEM_HOME="$HOME/Misc/gems"

export ZDOTDIR="$HOME/.config/zsh"
