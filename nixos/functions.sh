# Commands that are too complex for an alias (and perhaps too small for a
# stand-alone script

# Unzip all zip from the current directory into there dedicated directory
unzipall() {
    for ZIP in *.zip; do
        # Get file name and create directory if needed
        FILENAME=$(basename "$ZIP")
        DIRECTORY="${FILENAME%.zip}"

        if [ ! -d "${DIRECTORY}" ]; then
            # Create directory
            mkdir "${DIRECTORY}"
        fi

        # Unzip jar in directory
        unzip -d "${DIRECTORY}" "${ZIP}"
    done
}

# Display terminal colors
# http://askubuntu.com/q/27314
# http://askubuntu.com/a/611993
#
# This file echoes a bunch of color codes to the
# terminal to demonstrate what's available.  Each
# line is the color code of one forground color,
# out of 17 (default + 16 escapes), followed by a
# test use of that color on all nine background
# colors (default + 8 escapes).
termcolors(){
  T='gYw'   # The test text

  echo -e "\n                 40m     41m     42m     43m\
       44m     45m     46m     47m";

  for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
             '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
             '  36m' '1;36m' '  37m' '1;37m';
    do FG=${FGs// /}
    echo -en " $FGs \033[$FG  $T  "
    for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
      do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
    done
    echo;
  done
  echo
}

# gitignore.io
# Create useful .gitignore files for your project
# gi linux,osx,idris
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

# Open an article based on its key
function ppp() {
  KEY="*$1*"
  # `p "$(find ~/Sync/Papers/ -name $KEY -print)"`
  # find ~/Sync/Papers/ -iname $KEY -printf "'%p'\n"
  find /home/rfish/Sync/Papers/ -iname ${KEY} | sed "s/'/'\\\''/g" | sed "s/.*/'&'/g"
}

# Toggle the dpi settings in the `.Xresource`
function toggle-dpi() {
  grep -q '^!Xft\*dpi:' /home/rfish/.dotfiles/system/.Xresources
  if [ $? -eq 0 ]; then
    sed -i 's/^!Xft\*dpi:/Xft*dpi:/' /home/rfish/.dotfiles/system/.Xresources
    echo 'dpi activated'
  else
    sed -i 's/^Xft\*dpi:/!&/' /home/rfish/.dotfiles/system/.Xresources
    echo 'dpi deactivated'
  fi

  xrdb /home/rfish/.dotfiles/system/.Xresources
}
