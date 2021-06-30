
class RadComparator
    super Comparator

    var rads: Array[Int]

    redef type COMPARED: Int

    redef fun compare(a, b) do return rads[a] <=> rads[b]

end

var limit = 120000

var sieve = new Array[Bool]
var rads = new Array[Int]
for i in [0..limit[ do
    sieve.add(true)
    rads.add(1)
end
sieve[0] = false
sieve[1] = false

for i in [0..limit[ do
    if sieve[i] then
        rads[i] *= i
        var j = i + i
        while j < limit do
            sieve[j] = false
            rads[j] *= i
            j += i
        end
    end
end

var comparator = new RadComparator(rads)

var by_radical = new Array[Int].from([1..limit[)
comparator.sort(by_radical)

var total_sum = 0
for c in [1..limit[ do
    if rads[c] < c then
        for a in by_radical do
            var b = c - a
            if a >= b or b <= 0 then
                continue
            end
            if rads[c] * rads[a] * 2 > c then
                break
            end
            if a.gcd(b) == 1 then
                if rads[a] * rads[b] * rads[c] < c then
                    total_sum += c
                end
            end
        end
    end
end
print total_sum