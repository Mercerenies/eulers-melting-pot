#!/bin/bash

# Helper script for building i code.

# Run this from the root project folder (i.e. the eulers-melting-pot directory), NOT from etc/

mkdir -p tmp
(
    cd tmp
    cp ../problem126.i ./tmp.i
    ~/go/bin/it run
)
rm -r tmp/
