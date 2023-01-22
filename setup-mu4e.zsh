#!/usr/bin/env zsh

echo "\n<<< Starting mu4e Setup >>>\n"

if exists mu; then
    if exists mbsync; then
        if exists msmtp; then
            echo "mu, isync, and msmtp exist, continuing mu4e setup..."
            mbsync -aV
            mu init -m ~/.maildir --my-address evan_e@icloud.com --my-address evan@emcode.io --my-address evan.erksn@gmail.com --my-address ericenna@gmail.com --my-address eerickson@phasechange.ai
            mu index
        else
            echo "msmtp doesn't exist, please install..."
        fi
    else
        echo "isync doesn't exist, please install..."
    fi
else
    echo "mu doesn't exist, please install..."
fi
