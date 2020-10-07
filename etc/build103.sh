#!/bin/bash

# Run this from the root project folder (i.e. the eulers-melting-pot directory), NOT from etc/

mkdir tmp
cd tmp
cp ../problem103.dylan .
cat <<EOF >./problem103.lid
Library: problem103
Files: library
       problem103
EOF
cat <<EOF >./library.dylan
Module: dylan-user

define library problem103
  use common-dylan;
  use io;
end library problem103;

define module problem103
  use common-dylan;
  use format-out;
end module problem103;
EOF
dylan-compiler -build problem103.lid && ./_build/bin/problem103
cd ..
rm -r tmp/
