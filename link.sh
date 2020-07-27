#!/usr/bin/env bash

# http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html
# On nixos name the `.bash_profile` as `.xprofile`
# Reason: SLiM login manager doesn't call .bash_profile at login
# https://github.com/NixOS/nixpkgs/issues/5200
# https://bbs.archlinux.org/viewtopic.php?id=174916

# Link specific files, e.g., files prefixed by hotsname
function specific_links() {
    local prefix="$1"

    # Specialize links
    ln -v -frs "nixos/$prefix-configuration.nix" nixos/configuration.nix
    ln -v -frs "system/.i3/$prefix-i3status-rs.toml" system/.i3/i3status-rs.toml
}

specific_links $(hostname)

stow -v --target="${HOME}" system
stow -v --target="${HOME}" git
stow -v --target="${HOME}" spacemacs

sudo stow -v --target="/etc/nixos" nixos
