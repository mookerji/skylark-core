#!/usr/bin/env bash

set -e

echo "Testing Skylark Core library"
AWS_SECRET_ACCESS_KEY=foo AWS_ACCESS_KEY_ID=bar stack build skylark-core --test --coverage "$@"
