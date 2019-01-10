#!/usr/bin/python3

import alakazam as zz
from alakazam import _1, _2, _3, _4, _5

from collections import defaultdict

NUMBER = 5

cubes = []
cube_cache = defaultdict(lambda: 0)

def is_permutation(a, b):
    return sorted(str(a)) == sorted(str(b))

for i in zz.count(1).map(_1 ** 3):
    print(i)
    i1 = str(sorted(str(i)))
    cubes.append(i)
    cube_cache[i1] += 1
    if cube_cache[i1] == NUMBER:
        print(zz.of(cubes).filter(lambda x: is_permutation(x, i)).foldr_lazy(lambda x, _: x))
        break
