#!/bin/bash

cd problem90.ska

includes=''
for i in `find . -name '*.ska.png' -and -not -name 'main.ska.png' | sed 's/^..//'`; do
    includes="$includes -i $i"
done

echo python3 ../vendor/skastic/skastic.py --dump-ast $includes ./main.ska.png
python3 ../vendor/skastic/skastic.py --dump-ast $includes ./main.ska.png
