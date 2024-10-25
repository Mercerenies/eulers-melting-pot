#!/bin/bash

# Build script for problem184.L42

mkdir -p problem184/
cp problem184.L42 ./problem184/This.L42
(
    cd ./vendor/42/L42PortableLinux/
    ./L42.sh ../../../problem184
)
rm -r ./problem184
