#!/usr/bin/env bash

# http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html
# On nixos name the `.bash_profile` as `.xprofile`
# Reason: SLiM login manager doesn't call .bash_profile at login
# https://github.com/NixOS/nixpkgs/issues/5200
# https://bbs.archlinux.org/viewtopic.php?id=174916

# Link `specific.nix` file
case "$1" in
  home)
    ln -v -frs nixos/home-specific.nix nixos/specific.nix
    ln -v -frs system/.i3/home-i3status-rs.toml system/.i3/i3status-rs.toml
    ;;
  hp)
    ln -v -frs nixos/hp-specific.nix nixos/specific.nix
    ln -v -frs system/.i3/hp-i3status-rs.toml system/.i3/i3status-rs.toml
    ;;
  *)
    echo "Is this home or hp computer? recall with $0 home or $0 hp"
    exit 1
    ;;
esac

stow -v --target="$HOME" system
stow -v --target="$HOME" git
stow -v --target="$HOME" spacemacs

sudo stow -v --ignore=".*-specific.nix" --target="/etc/nixos" nixos
