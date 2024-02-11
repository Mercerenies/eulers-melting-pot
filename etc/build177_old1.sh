#!/bin/bash

# Build script for Tyr (Problem 177)
#
# Removing Tyr from my list. I can't get "Hello World" to run.

(
    mkdir -p problem177/src
    cp problem177.tyr problem177/src/
    cd problem177
    cat <<EOF >package.draupnir
src=src
name=Problem177
entryPoint=Problem177.hello.myMain
EOF
    ../vendor/tyr/draupnir run
)
#rm -rf problem177/
