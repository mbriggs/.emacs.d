#!/bin/bash

# Define directory and repository variables
DIRECTORY=".copilot"
REPO_URL="https://github.com/copilot-emacs/copilot.el"

# Check if .copilot directory exists
if [ ! -d "$DIRECTORY" ]; then
    # Clone the repository if the directory does not exist
    git clone "$REPO_URL" "$DIRECTORY"
    CLONE_STATUS=$?
    ACTION="cloned"
else
    # Update the repository if the directory exists
    cd "$DIRECTORY" && git pull
    UPDATE_STATUS=$?
    if [ $UPDATE_STATUS -eq 0 ]; then
        ACTION="updated"
    else
        ACTION="already up to date"
    fi
fi

# Check if node exists on PATH
if ! command -v node &> /dev/null; then
    echo -e "\033[31mError: Node.js is not installed or not found on PATH.\033[0m"
    exit 1
fi

# Check if node version is v18 or higher
NODE_VERSION=$(node -v)
NODE_MAJOR_VERSION=$(echo $NODE_VERSION | cut -d '.' -f1 | sed 's/v//')
if [ "$NODE_MAJOR_VERSION" -lt 18 ]; then
    echo -e "\033[31mError: Node.js version is less than 18. Found version: $NODE_VERSION\033[0m"
    exit 1
fi

# Get node executable location
NODE_EXECUTABLE=$(which node)

# Print success message
echo -e "\033[32mSuccess! copilot.el was $ACTION. \nNode executable location: $NODE_EXECUTABLE \nNode Version: $NODE_VERSION\033[0m"

