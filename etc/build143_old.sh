#!/bin/bash

# Build script for problem143.L42, which was not fast enough for our
# purposes. This script is not used.

mkdir -p problem143/
cp problem143.L42 ./problem143/This.L42
(
    cd ./vendor/42/L42PortableLinux/
    ./L42.sh ../../../problem143
)
rm -r ./problem143
