# -- bash_rc for interactive shell
# https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789
#
# http://unix.stackexchange.com/a/50667 ; man bash
# Gentle reminder about bash:
# - *Interactive* :: As the term implies: Interactive means that the
#     commands are run with user-interaction from keyboard. E.g. the
#     shell can prompt the user to enter input.
# - `~/.bashrc` :: The individual per-interactive-shell startup file
#
# - Login :: Means that the shell is run as part of the login of the
#     user to the system. Typically used to do any configuration that
#     a user needs/wants to establish his work-environment.
# - `/etc/profile` :: The systemwide initialization file, executed for
#     login shells.
# - `~/.bash_profile` (or old `~/.profile`) :: The personal
#     initialization file, executed for login shells.

# Source dotfiles (order matters)
# alias: shortcuts for commands
# prompt: custom prompt
for DOTFILE in "$DOTFILES_DIR"/bash/.{function,alias,prompt}; do
    [ -f "$DOTFILE" ] && . "$DOTFILE"
done

# OSX specific
if [ "${OS}" = "OSX" ]; then
    . /usr/local/etc/bash_completion.d/git-prompt.sh
    for DOTFILE in "$DOTFILES_DIR"/bash/.alias.osx; do
        [ -f "$DOTFILE" ] && . "$DOTFILE"
    done
fi
