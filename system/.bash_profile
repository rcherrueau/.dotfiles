# -- bash_profile for login shell
# https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789
#
# http://unix.stackexchange.com/a/50667 ; man bash
# Gentle reminder about bash:
# - *Login* :: Means that the shell is run as part of the login of the
#     user to the system. Typically used to do any configuration that
#     a user needs/wants to establish his work-environment.
# - `/etc/profile` :: The systemwide initialization file, executed for
#     login shells.
# - `~/.bash_profile` (or old `~/.profile`) :: The personal
#     initialization file, executed for login shells.
#
# - Interactive :: As the term implies: Interactive means that the
#     commands are run with user-interaction from keyboard. E.g. the
#     shell can prompt the user to enter input.
# - `~/.bashrc` :: The individual per-interactive-shell startup file

# OS
if [ "$(uname -s)" = "Darwin" ]; then
    OS="OSX"
else
    OS=`uname -s`
fi
export OS="$OS"

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
export DOTFILES_DIR="$( cd -P "$( dirname "$SOURCE" )/.." && pwd )"

# Source dotfiles (order matters)
# functions: commands that are too complex for an alias
# env: environment variables
for DOTFILE in "$DOTFILES_DIR"/bash/.{function,env}; do
    [ -f "$DOTFILE" ] && . "$DOTFILE"
done

# OSX specific
if [ ${OS} = "OSX" ]; then
    for DOTFILE in "$DOTFILES_DIR"/bash/.{env}.osx; do
        [ -f "$DOTFILE" ] && . "$DOTFILE"
    done
fi

# NixOS specific initlializations
if [ "${OS}" = "Linux" ]; then
  sh ~/.fehbg &
  sh ~/.xcape &
fi

