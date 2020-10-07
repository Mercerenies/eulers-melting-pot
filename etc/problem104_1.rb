
CAP = 10 ** 9

$sqrt5 = Math.sqrt 5
$phi = (1 + $sqrt5) / 2
$psi = (1 - $sqrt5) / 2

$logphi = Math.log10($phi)
$log5 = Math.log10($sqrt5)

def fibo
  a = 1
  b = 1
  Enumerator.new do |y|
    loop do
      y << a
      a, b = b, (a + b) % CAP
    end
  end
end

def explicit_fibo(i)
  # The explicit formula is
  #
  # fibo(i) = ($phi ** i - $psi ** i) / $sqrt5
  #
  # For the magnitudes we're dealing with, $psi ** i becomes trivial
  # incredibly fast, so we omit it. But this massively overflows
  # floating-point arithmetic really quickly. So we're going to
  # calculate the log
  #
  # log(fibo(i)) = i log($phi) - log($sqrt5)
  log = i * $logphi - $log5
  # Shifting left or right in base 10 doesn't change the digits. Since
  # we're calculating things in log base 10, that corresponds to
  # adding or subtracting integers from the log. So let's make our
  # number of reasonable magnitude, then do 10^x on the result.
  10 ** (log.modulo(1.0) + 11)
end

def is_pan x
  y = x.chars.sort.uniq
  y[0] == "1" && y[-1] == "9" && y.size == 9
end

fibo.lazy.map(&:to_s).with_index(1).each do |x, i|
  next if x.size < 9
  head = explicit_fibo(i).to_i.to_s[0..8]
  tail = x.to_s[-9..]
  if is_pan head and is_pan tail
    puts i
    break
  end
end
