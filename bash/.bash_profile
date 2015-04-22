# https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789
# If not running interactively, don't do anything
[ -z "$PS1"] && return

# OS
if [ "$(uname -s)" = "Darwin" ]; then
    OS="OSX"
else
    OS=`uname -s`
fi
export ${OS}

# Source dotfiles (order matters)
# function: commands that are too complex for an alias
# env: environment variables
# alias: shortcuts for commands
# prompt: custom prompt
DOTFILES_DIR="$HOME/.dotfiles"

for DOTFILE IN $DOTFILES_DIR"/system/.{function,env,alias,prompt}"; do
    [ -f "$DOTFILE"] && source "$DOTFILE"
done

# OSX specific
for DOTFILE IN $DOTFILES_DIR"/system/.{env,alias,custom}.osx"; do
    [ -f "$DOTFILE"] && source "$DOTFILE"
done
