#!/usr/bin/env bash

set -e

for dir in src test
do
    find $dir -name "*.hs" | xargs stylish-haskell -i
done


