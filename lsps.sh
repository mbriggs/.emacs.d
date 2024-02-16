#!/bin/bash

set -e

gem install ruby-lsp
gem install solargraph
npm install -g yaml-language-server
npm install -g typescript typescript-language-server
npm install -g vscode-langservers-extracted
npm install -g dockerfile-language-server-nodejs
npm install -g bash-language-server
npm install -g vscode-css-languageserver-bin
npm install -g vscode-html-languageserver-bin
npm install -g vscode-json-languageserver
pip3 install cmake-language-server
go install golang.org/x/tools/gopls@latest
