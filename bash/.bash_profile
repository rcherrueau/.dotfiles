# https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789
# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# OS
if [ "$(uname -s)" = "Darwin" ]; then
    OS="OSX"
else
    OS=`uname -s`
fi
export "$OS"

# Location of ".dotfiles/"
# http://stackoverflow.com/a/246128/2072144
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
    # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    # if $SOURCE was a relative symlink, we need to resolve it relative to the
    # path where the symlink file was located
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
DOTFILES_DIR="$( cd -P "$( dirname "$SOURCE" )/.." && pwd )"

# Source dotfiles (order matters)
# functions: commands that are too complex for an alias
# env: environment variables
# alias: shortcuts for commands
# prompt: custom prompt
for DOTFILE in "$DOTFILES_DIR"/system/.{function,env,alias,prompt}; do
    [ -f "$DOTFILE" ] && source "$DOTFILE"
done

# OSX specific
if [ ${OS} = "OSX" ]; then
    for DOTFILE in "$DOTFILES_DIR"/system/.{env,alias,custom}.osx; do
        [ -f "$DOTFILE" ] && source "$DOTFILE"
    done
fi
