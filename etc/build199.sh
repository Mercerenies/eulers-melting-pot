#!/bin/bash

# Build script for ChoiceScript. Run from Euler's Melting Pot project
# root.
#
# Assumes that the ChoiceScript engine
# (https://github.com/dfabulich/choicescript/) is cloned at
# `./vendor/choicescript`.

(
    set -x

    CHOICESCRIPT_DIR='./vendor/choicescript/'
    SCENES_DIR=$CHOICESCRIPT_DIR'web/mygame/scenes'
    cp problem199.txt $SCENES_DIR/startup.txt

    cd $CHOICESCRIPT_DIR
    node serve
    # 
)
