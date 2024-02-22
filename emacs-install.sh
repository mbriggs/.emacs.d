#!/usr/bin/env bash

set -e

echo "compiling emacs from source"
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp --with-xwidgets --with-modern-asingh4242-icon
echo "aliasing the emacs app to /applications"
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications"'
echo "disabling C-M-d"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
echo "installing fonts"
cp ./font/*.ttf ~/Library/Fonts
echo "cloning copilot.el"
. ./copilot.sh
echo "installing lsp servers"
. ./lsps.sh
echo "installing formatters"
. ./fmt.sh
echo "done"
