#!/bin/bash

# Helper script for building problem134.dfy
#
# We use the Java target backend because I found that to be the most
# straightforward to work with.

shopt -s globstar

./vendor/dafny_bin/dafny-3.2.0-x64-debian-8.11/dafny/dafny /compileTarget:java problem134.dfy &&
    javac -cp ./vendor/dafny_bin/dafny-3.2.0-x64-debian-8.11/dafny/DafnyRuntime.jar problem134-java/**/*.java &&
    java -cp ./problem134-java:./vendor/dafny_bin/dafny-3.2.0-x64-debian-8.11/dafny/DafnyRuntime.jar problem134
rm -r problem134-java
