#!/bin/bash

# Helper script for building Gleam code.

# Run this from the root project folder (i.e. the eulers-melting-pot directory), NOT from etc/

mkdir -p tmp/src
mkdir -p tmp/test
(
    cd tmp
    cp ../problem119.gleam ./src
    cat <<EOF >./gleam.toml
name="problem119"
tool="gleam"
EOF
    ../vendor/gleam/gleam build
    cd ./_build/default/lib/problem119/src
    erl -compile problem119
    erl -noshell -s problem119 main -s init stop
)
rm -r tmp/
