#!/bin/bash

# Whiley build script. Assumes `wy` is on your path (if you did `cargo
# install whiley` then it is)

set -x

(
    mkdir -p problem208/src
    cp problem208.whiley problem208/src/main.whiley
    cd problem208
    cat <<EOF >./wy.toml
[package]
name="problem208"
authors=["Silvio Mayolo"]
version="0.1.0"

[build]
platforms=["whiley"]

[build.whiley]
source="src"
target="."
verify=false

[dependencies]
std="0.3.2"
EOF
    wy build
)

rm -r problem208/
