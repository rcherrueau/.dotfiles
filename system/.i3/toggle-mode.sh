#!/usr/bin/env bash

# Toggle between laptop and dell-desktop mode
function toggle-screen() {
    grep -q '^!Xft\*dpi:' /home/rfish/.dotfiles/system/.Xresources
    if [ $? -eq 0 ]; then
        set-laptop-mode.sh 'laptop'
    else
        set-laptop-mode.sh 'dell-desktop'
    fi
}
