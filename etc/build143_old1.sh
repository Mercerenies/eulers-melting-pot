#!/bin/bash

# Runner script for AssemblyScript

mkdir -p problem143/
cp problem143.ts ./problem143/problem143.ts

user_interrupt() {
    rm -r ./problem143
    exit
}
trap user_interrupt SIGINT

cat <<EOF >./problem143/package.json
{
  "main": "problem143.js",
  "devDependencies": {
    "assemblyscript": "^0.19.23",
    "serve": "^14.2.0"
  },
  "scripts": {
    "asbuild:release": "asc problem143.ts --target release",
    "start": "npx serve ."
  }
}
EOF

cat <<EOF >./problem143/index.html
<!DOCTYPE html>
<html lang="en">
<head>
<script type="module">
import { main } from './problem143.js';
document.body.innerText = main();
</script>
</head>
<body></body>
</html>
EOF

NPM=/usr/local/bin/npm
NPX=/usr/local/bin/npx
NODE=/usr/local/bin/node

(
    cd problem143/
    $NPM install
    ./node_modules/.bin/asc --jsFile problem143.js problem143.ts
    # AssemblyScript imports this module that doesn't seem to exist,
    # and we don't use it, so just mock it ¯\_(ツ)_/¯
    sed -i '1 cwindow.env = {abort() {}}; window.abort = env.abort;' ./problem143.js
    $NPM start
)
