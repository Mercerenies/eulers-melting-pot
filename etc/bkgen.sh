#!/bin/bash

# Helper script for generating and running the Problem 109 image file

python3 ./etc/bkgen.py ./problem109.png ./etc/problem109.bf && python3 ./vendor/Befunk/befunk.py <<EOF
./problem109.png
n
EOF
