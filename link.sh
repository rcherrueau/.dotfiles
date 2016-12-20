#!/bin/sh
# http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html
# On nixos name the `.bash_profile` as `.xprofile`
# Reason: SLiM login manager doesn't call .bash_profile at login
# https://github.com/NixOS/nixpkgs/issues/5200
# https://bbs.archlinux.org/viewtopic.php?id=174916

stow --target="$HOME"       system
stow --target="$HOME"       git
stow --target="$HOME"       spacemacs
stow --target="/etc/nixos/" nixos

# TODO: case home/hp ln -s specific
