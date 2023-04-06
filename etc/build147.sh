#!/bin/bash

# Build script for the Lean solution to Problem 147.

(
    leanproject new problem147
    #mkdir -p problem147/src
    cp problem147.lean ./problem147/src/
    cd problem147
    lean ./src/problem147.lean
)
rm -rf problem147
