
from collections import defaultdict
import math

def letters_multi(word):
    result = defaultdict(lambda: 0)
    for x in word:
        result[x] += 1
    return result

# Tries to find a substitution which replaces the word with the square
# number in question. Assumes the word and the square have the same
# length (check this precondition in advance)
def try_substitution(word, square):
    assert(len(word) == len(square))
    used_digits = set()
    substitution = {}
    for i in range(len(word)):
        if word[i] not in substitution:
            if square[i] in used_digits:
                return None
            substitution[word[i]] = square[i]
            used_digits.add(square[i])
        elif substitution[word[i]] != square[i]:
            return None
    return substitution

def do_sub(word, sub):
    return ''.join(map(lambda x: sub[x], word))

MAX_LENGTH = 14
MAX_SQUARE = 10 ** MAX_LENGTH

with open('./files/p098_words.txt') as f:
    words = f.readline().split(',')
    words = list(map(lambda x: x[1:-1], words))

bag_of_letters = {}
for word in words:
    bag_of_letters[word] = letters_multi(word)

biggest = 0
for word1 in words:
    for word2 in words:
        if bag_of_letters[word1] == bag_of_letters[word2] and word1 != word2:
            # Nontrivial anagrams
            for i in range(math.floor(math.sqrt(10 ** (len(word1) - 1))), math.ceil(math.sqrt(10 ** len(word1)))):
                s = i * i
                square = str(i * i)
                if len(square) != len(word1):
                    continue
                res = try_substitution(word1, square)
                if res:
                    sub = int(do_sub(word2, res))
                    if math.sqrt(sub) == math.floor(math.sqrt(sub)):
                        if len(str(sub)) == len(square):
                            biggest = max(biggest, s)
print(biggest)
