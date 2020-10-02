#!/usr/bin/env bash

# Let me switch between laptop and desktop mode for my hp using rofi.

case "$1" in
  IMT)
      sed -i 's/^Xft\*dpi:/!&/' /home/rfish/.dotfiles/system/.Xresources
      xrdb /home/rfish/.dotfiles/system/.Xresources
      xrandr --output DP1-1 --auto --output eDP1 --off
      feh --bg-fill $HOME/.background-image
      >&2 i3 restart
      sudo rfkill block wifi
      >&2 echo 'IMT mode activated'
      ;;
  laptop)
      sed -i 's/^!Xft\*dpi:/Xft*dpi:/' /home/rfish/.dotfiles/system/.Xresources
      xrdb /home/rfish/.dotfiles/system/.Xresources
      xrandr --output eDP1 --auto --output DP1-1 --off
      feh --bg-fill $HOME/.background-image
      >&2 i3 restart
      sudo rfkill unblock wifi
      >&2 echo 'hp laptop mode activated'
      ;;
  *)
      echo "IMT"
      echo "laptop"
      ;;
esac
