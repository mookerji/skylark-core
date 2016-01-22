#!/usr/bin/env bash

set -e

function install_deps () {
    wget http://dl.ambiweb.de/mirrors/ftp.i-scream.org/libstatgrab/libstatgrab-0.90.tar.gz
    tar xvf libstatgrab-0.90.tar.gz
    cd libstatgrab-0.90
    ./configure
    make
    sudo make install
    cd ../
}

if [[ "$OSTYPE" == "linux-"* ]]; then
    sudo apt-get install libstatgrab
elif [[ "$OSTYPE" == "darwin"* ]]; then
    install_deps_osx
else
    exit 1
fi

echo "Installing deps..."
install_deps
