
# This is insane. If this works, I'm crazy.

def _all_prods(n, m)
  if n == 1
    [[]]
  else
    arr = []
    (m..n).each do |d|
      if n % d == 0
        sub = _all_prods(n / d, d)
        sub.each { |curr| curr << d }
        arr += sub
      end
    end
    arr
  end
end

def all_prods(n)
  _all_prods(n, 2)
end

$optimal_cases = {}
$remaining = 12000

def solve_for(n)
  all_prods(n).each { |terms|
    # The question is: how many 1's do we need to pad this structure
    # with so the sum and product are equal?
    sum = terms.sum
    next if sum > n
    padding = n - sum
    total_terms = terms.length + padding
    if total_terms <= 12000 and not $optimal_cases.has_key? total_terms
      $optimal_cases[total_terms] = n
      $remaining -= 1
    end
  }
end

i = 1
while $remaining > 0
  solve_for i
  i += 1
end

puts (2..12000).map { |x| $optimal_cases[x] }.sort.uniq.sum
