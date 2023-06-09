#!/bin/bash

# Set this to where you downloaded pikt and Koltin
PIKT_PATH=./vendor/pikt
KOTLIN_PATH=( ~/Downloads/kotlinc-1.7.20/kotlinc/bin/kotlinc )

COMMAND='-interpret'
if [ "$2" == "dump" ]; then
    COMMAND='-printoutput'
fi

java "-Dcolors=$PIKT_PATH/core/src/main/resources/colors" "-Dsource=$1" "-Dlib=$PIKT_PATH/stdlib/target/stdlib.jar" "-Djvmcompiler=$KOTLIN_PATH" -jar "$PIKT_PATH/core/target/pikt.jar" "$COMMAND"
