#!/usr/bin/env bash

set -e

for dir in src test/Test
do
    hlint $dir
done


