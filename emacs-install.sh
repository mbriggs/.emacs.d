#!/usr/bin/env bash

set -e

brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp --with-xwidgets --with-modern-asingh4242-icon
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications"'
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
cp ./font/*.ttf ~/Library/Font
. ./copilot.sh
. ./lsps.sh
. ./fmt.sh
