#!/bin/bash

# Build script for Whiley (Problem 177)
#
# UNUSED: Whiley doesn't support non-integer operations. We can't use
# it for 177.

(
    mkdir -p problem177/src
    cp problem177.whiley problem177/src/main.whiley
    cd problem177
    cat <<EOF >wy.toml
[package]
name="problem177"
authors=["Mercerenies"]
version="0.1.0"

[build]
platforms=["whiley"]

[dependencies]
std="0.3.2"
EOF
    wy build && wy run
)
rm -rf problem177/
