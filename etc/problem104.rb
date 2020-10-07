
def fibo
  a = 1
  b = 1
  Enumerator.new do |y|
    loop do
      y << a
      a, b = b, a + b
    end
  end
end

def is_pan x
  y = x.chars.sort.uniq
  y[0] == "1" && y[-1] == "9" && y.size == 9
end

fibo.lazy.map(&:to_s).with_index(1).each do |x, i|
  next if x.size < 9
  head = x.to_s[0..8]
  tail = x.to_s[-9..]
  if is_pan head and is_pan tail
    puts i
    break
  end
end
