#!/usr/bin/env bash

set -e

echo "Installing deps..."

if [[ "$OSTYPE" == "linux-"* ]]; then
    sudo apt-get install libstatgrab
elif [[ "$OSTYPE" == "darwin"* ]]; then
    brew install libstatgrab
else
    wget http://dl.ambiweb.de/mirrors/ftp.i-scream.org/libstatgrab/libstatgrab-0.90.tar.gz
    tar xvf libstatgrab-0.90.tar.gz
    (cd libstatgrab-0.90; ./configure && make && sudo make install)
fi

