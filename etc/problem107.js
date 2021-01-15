
const fs = require('fs');
const lines = fs.readFileSync('./files/p107_network.txt', 'utf8').split("\n").map((x) => x.split(",")).slice(0, -1);

for (line of lines) {
  for (var i = 0; i < line.length; i++) {
    if (line[i] == '-') {
      line[i] = null;
    } else {
      line[i] = parseInt(line[i], 10);
    }
  }
}

// Figure out the current cost (without removing any edges).
var total_cost = 0;
for (var i = 0; i < lines.length; i++) {
  for (var j = 0; j < lines.length; j++) {
    if (lines[i][j] != null) {
      total_cost += lines[i][j];
    }
  }
}
// We double counted each vertex.
total_cost /= 2;

// Basically, Prim's algorithm.
var optimized_cost = 0;
const adjoined = {0: true};
for (var i = 0; i < lines.length - 1; i++) {
  var candidate = null;
  var candidate_vertex = null;
  for (var j = 0; j < lines.length; j++) {
    if (adjoined[j]) {
      const line = lines[j];
      for (var k = 0; k < lines.length; k++) {
        if ((line[k] != null) && (!adjoined[k])) {
          if ((candidate == null) || (candidate > line[k])) {
            candidate = line[k];
            candidate_vertex = k;
          }
        }
      }
    }
  }
  adjoined[candidate_vertex] = true;
  optimized_cost += candidate;
}

console.log(total_cost - optimized_cost);
