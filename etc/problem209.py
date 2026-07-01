
'''We are analyzing functions from Bool^6 to Bool. Consider the domain
set, whose size is 2^6 = 64. We will treat the input as a number
between 0 and 63 (where each bit in the binary representation is the
corresponding input value). The problem permutes (a, b, c, d, e, f) to
(b, c, d, e, f, a XOR (b AND c)). Call this permutation function p :
Bool^6 -> Bool^6.

First Lemma: p is a bijection. This is straightforward to see. If
p(abcdef) = p(a'b'c'd'e'f') then b = b', c = c', d = d', e = e', f =
f' directly. And it's a very straightforward argument (by the last
value) that a = a'. Thus, p is injective, so pigeonhole principle
tells us it's bijective.

The constraint given in the problem, which we'll call constraint Q,
is, for our function τ and any input w, τ(w) AND τ(p(w)) = 0. Put
another way, if τ(w) is 1 then τ(p(w)) must be 0.

p is a bijection on a finite set, so it can be split into its cycles.
We directly compute those below, and find (by computation) that there
are six cycles:

Component 0: [1]: {0}
Component 1: [6]: {1, 2, 4, 8, 16, 32}
Component 2: [46]: (everything else)
Component 3: [6]: {5, 10, 20, 40, 17, 34}
Component 4: [3]: {9, 18, 36}
Component 5: [2]: {21, 42}

We can consider the cycles independently when counting the number of
functions τ (since Q is a constraint placed on τ's treatment of p, and
the cycles characterize p). And then we multiply the resulting counts
together at the end.

For a cycle of length n, we want to count the number of possible
outputs for this function on elements of that cycle. Without the
constraint Q, we want to count the number of n-digit binary numbers,
which is clearly 2^n. With constraint Q, we want to count the number
of n-digit binary numbers that do not have 2 consecutive 1's (Q₁), AND
that the first and last digit cannot both be ones (Q₂). Q₁ is actually
very easy to count. It's just the Fibonacci sequence (seeded at (1,
2)). And clearly Q₂ is a stricter constraint than Q₁.

Let f(n) be the number of binary n-digit numbers satisfying Q₂. The
degenerate cases are: f(0) = 1, f(1) = 1, f(2) = 3 (by direct
hand-calculation). For n >= 3, we can either place a zero at the first
slot (which breaks the cycle and does not constraint anything) or a
one (which breaks the cycle but constrains *two* other slots), so

f(n) = fibo(n - 1) + fibo(n - 3)

a slightly perturbed version of the Fibonacci sequence.

Then the answer is just

f(1) * f(6) * f(46) * f(6) * f(3) * f(2)

'''

#import matplotlib.pyplot as plt
import networkx as nx

# (a, b, c, d, e, f) treated as a binary six-digit integer abcdef.
type Inputs = int


def permute(x: Inputs) -> Inputs:
    '''Convert abcdef to bcdef(a^(b&c)).'''
    a = (x >> 5) & 1
    b = (x >> 4) & 1
    c = (x >> 3) & 1
    new_x = (x & 0b011111) * 2 + (a ^ (b & c))
    return new_x


def fibo(n):
    a = 1
    b = 2
    for i in range(0, n):
        a, b = b, a + b
    return a


def count_for_component(cycle_length):
    if cycle_length < 3:
        return [1, 1, 3][cycle_length]
    return fibo(cycle_length - 1) + fibo(cycle_length - 3)


for i in range(0, 64):
    print(i, permute(i))

g = nx.DiGraph()
for i in range(0, 64):
    g.add_node(i)

for i in range(0, 64):
    g.add_edge(i, permute(i))

for i, component in enumerate(nx.weakly_connected_components(g)):
    print(f"Component {i}: [{len(component)}]: {component}")

cycle_lengths = [len(c) for c in nx.weakly_connected_components(g)]
print(cycle_lengths)

print([fibo(n) for n in range(10)])
total = 1
for cycle in cycle_lengths:
    total *= count_for_component(cycle)
print(f"Total: {total}")


#nx.draw(g, with_labels=True)
#plt.show()
