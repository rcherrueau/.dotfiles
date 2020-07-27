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

# Find an article based on its title
function fp() {
  KEY="*$1*"
  # `p "$(find ~/Sync/Papers/ -name $KEY -print)"`
  # find ~/Sync/Papers/ -iname $KEY -printf "'%p'\n"
  find /home/rfish/Sync/Papers/ -iname ${KEY} | sed "s/'/'\\\''/g" | sed "s/.*/'&'/g"
}

# Make xdg-open runs in background
function xdg-open-background() {
  xdg-open "$*" &
}

function rtfm() {
  man $@ || xdg-open "https://www.qwant.com/?q=$@"; 
}

#Make your PDF look scanned.
#Idea from the post in HN. Make a Gist with the command code.
#https://gist.github.com/jduckles/29a7c5b0b8f91530af5ca3c22b897e10
function scanned-pdf() {
  local INPUT_FILE=$1
  local OUTPUT_FILE=${2:-output-scanned.pdf} 
  convert -density 150 ${INPUT_FILE} -colorspace gray -linear-stretch 3.5%x10% -blur 0x0.5 -attenuate 0.25 +noise Gaussian  -rotate 1.0  aux_output.pdf
  gs -dSAFER -dBATCH -dNOPAUSE -dNOCACHE -sDEVICE=pdfwrite -sColorConversionStrategy=LeaveColorUnchanged -dAutoFilterColorImages=true -dAutoFilterGrayImages=true -dDownsampleMonoImages=true -dDownsampleGrayImages=true -dDownsampleColorImages=true -sOutputFile=${OUTPUT_FILE} aux_output.pdf
  rm aux_output.pdf
}
