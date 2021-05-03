
concept g(x, y, z, k) {
  return 2 * (x * z + y * z + x * y + 2 * k * (x + y + z + k - 1))
}

software {
  limit = 40000
  c = []
  for each 1 to limit
    c += 0
  end

  z = 1
  loop {
    if g(z, z, z, 0) > limit
      break
    end
    y = z
    loop {
      if g(y, y, z, 0) > limit
        break
      end
      x = y
      loop {
        if g(x, y, z, 0) > limit
          break
        end
        k = 0
        loop {
          if g(x, y, z, k) > limit
            break
          end
          if g(x, y, z, k) > -1
            c[g(x, y, z, k)] = c[g(x, y, z, k)] + 1
          end
          k += 1
        }
        x += 1
      }
      y += 1
    }
    z += 1
  }

  k = 1
  loop {
    if c[k] = 1000
      print(k)
      break
    end
    k += 1
  }
}
