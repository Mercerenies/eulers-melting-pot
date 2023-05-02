
# Brute force with culling of bad branches, too slow to even run the
# test case (with limit 45) shown in the problem.

gcd = (a, b) ->
  while b != 0n
    [a, b] = [b, a % b]
  a

class Fraction

  constructor: (@numerator, @denominator) ->

  toString: ->
    "#{@numerator}/#{@denominator}"

  add: (that) ->
    new Fraction(
      this.numerator * that.denominator + that.numerator * this.denominator
      this.denominator * that.denominator
    ).simplify()

  negate: ->
    new Fraction(- @numerator, @denominator)

  sub: (that) ->
    this.add that.negate()

  # Applies in-place
  simplify: ->
    d = gcd(@numerator, @denominator)
    @numerator /= d
    @denominator /= d
    if @denominator < 0
      @numerator *= -1
      @denominator *= -1
    this

  toDouble: ->
    Number(@numerator) / Number(@denominator)

  compareTo: (that) ->
    this.numerator * that.denominator - that.numerator * this.denominator

squares = (new Fraction(1n, BigInt(x * x)) for x in [2..45]);
half = new Fraction(1n, 2n)

bruteForce = (currentSum, indexToRemove) ->
  console.log indexToRemove if indexToRemove < 10
  if currentSum.compareTo(half) == 0
    # Current sum is a solution, so count it
    1
  else if indexToRemove >= squares.length
    # Base case; we're at the end of the list.
    0
  else if currentSum.compareTo(half) < 0
    # Current sum is too small, so short-circuit out.
    0
  else
    # Try removing the index at the current position and recurse; also
    # try *not* removing it.
    bruteForce(currentSum.sub(squares[indexToRemove]), indexToRemove + 1) + bruteForce(currentSum, indexToRemove + 1)

totalSum = squares.reduce (x, y) -> x.add(y)
console.log bruteForce(totalSum, 0)
