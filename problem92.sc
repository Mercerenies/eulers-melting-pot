
var act, total, cache;

cache = Array.fill(600, { -1 });

act = { arg i;
  if ((i < 600).and { cache.at(i) > -1 }) {
    cache.at(i)
  } {
    if (i == 1) {
      0
    } {
      if (i == 89) {
        1
      } {
        var next = 0;
        var digits = Array.newFrom(i.asStringToBase);
        var result = act.(digits.collect { arg x; x.digit * x.digit; }.sum);
        if (i < 600) {
          cache.put(i, result);
        };
        result
      }
    }
  }
};

total = 0;
1.for(9999999, { arg i;
  total = total + act.(i);
});
total.postln;
