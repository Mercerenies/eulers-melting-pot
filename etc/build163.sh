#!/bin/bash

# Runner script for AssemblyScript

mkdir -p problem163/
cp problem163.ts ./problem163/problem163.ts

user_interrupt() {
    rm -r ./problem163
    exit
}
trap user_interrupt SIGINT

cat <<EOF >./problem163/package.json
{
  "main": "problem163.js",
  "devDependencies": {
    "assemblyscript": "^0.19.23",
    "serve": "^14.2.0"
  },
  "scripts": {
    "asbuild:release": "asc problem163.ts --target release",
    "start": "npx serve ."
  }
}
EOF

cat <<EOF >./problem163/index.html
<!DOCTYPE html>
<html lang="en">
<head>
<script type="module">
import { main } from './problem163.js';
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
    cd problem163/
    $NPM install
    ./node_modules/.bin/asc --jsFile problem163.js problem163.ts
    # AssemblyScript imports this module that doesn't seem to exist,
    # and we don't use it, so just mock it ¯\_(ツ)_/¯
    sed -i '1 cwindow.env = {abort() {}}; window.abort = env.abort;' ./problem163.js
    $NPM start
)
