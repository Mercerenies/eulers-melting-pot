#!/bin/bash

# Run from the parent directory; invoke with argument being MASM directory

dir="$1"

if "$dir/bin/ml" -c -coff problem41.asm; then
    "$dir/bin/link" problem41.obj `cygpath -w "$dir/lib/kernel32.lib"` /subsystem:console /entry:start
fi
