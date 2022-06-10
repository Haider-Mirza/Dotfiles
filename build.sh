#!/usr/bin/env bash
#  _   _ __  ___
# | | | |  \/  | My Website: haider.gq
# | |_| | |\/| | My Email: haider@haider.gq
# |  _  | |  | | My Github: https://github.com/Haider-Mirza
# |_| |_|_|  |_| My IRC username: haider in Libera.Chat

# This script builds my dotfiles
# Dependancies: Stow

echo -e "Make sure that Stow is installed\n"
stow --target="$HOME" --dir="$HOME/dotfiles/stow" .
