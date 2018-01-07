
class Helper {

  static sortImpl(list, a, b) {
    if (a >= b - 1) {
      return null
    }
    var j = a
    for (i in (a + 1) .. (b - 1)) {
      if (list[i] < list[a]) {
        j = j + 1
        var temp = list[i]
        list[i] = list[j]
        list[j] = temp
      }
    }
    var temp = list[j]
    list[j] = list[a]
    list[a] = temp
    sortImpl(list, a, j)
    sortImpl(list, j + 1, b)
  }

  static sort(list) {
    sortImpl(list, 0, list.count)
  }

  static characters(arg) {
    return arg.toString.bytes.toList
  }

  static listEq(a, b) {
    if (a.count != b.count) {
      return false
    }
    var a1 = null
    var b1 = null
    while ((a1 = a.iterate(a1)) && (b1 = b.iterate(b1))) {
      if (a.iteratorValue(a1) != b.iteratorValue(b1)) {
        return false
      }
    }
    return true
  }

}

var i = 1
while (true) {
  var okay = true
  var list0 = Helper.characters(i)
  Helper.sort(list0)
  for (n in 2..6) {
    var list = Helper.characters(i * n)
    Helper.sort(list)
    if (!Helper.listEq(list0, list)) {
      okay = false
    }
  }
  if (okay) {
    System.print(i)
    break
  }
  i = i + 1
}
