#!/bin/sh
# http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html

stow --target="$HOME" bash
stow --target="$HOME" git
stow --target="$HOME" spacemacs
