#!/usr/bin/env bash

set -e

echo "Building Skylark Core"
stack build skylark-core "$@"

