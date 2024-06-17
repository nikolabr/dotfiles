# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
XDG_DATA_DIRS="$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS"
