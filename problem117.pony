
actor Main
  new create(env: Env) =>
    var gray = Array[I64](51)
    var col = Array[I64](51)
    gray.push(1)
    col.push(1)
    var i: I32 = 1
    while i < 51 do
      var g = get(col, i - 2) + get(col, i - 3) + get(col, i - 4)
      gray.push(g)

      var r: I64 = 0
      var j: I32 = 0
      while j < i do
        r = r + get(gray, j)
        j = j + 1
      end
      col.push(r + g)

      i = i + 1
    end

    env.out.print(get(col, 50).string())

  fun get(arr: Array[I64], index: I32): I64 =>
    try
      arr(USize.from[I32](index))?
    else
      0
    end
