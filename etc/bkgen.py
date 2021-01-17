
import cv2 as cv
import numpy as np
import alakazam as zz
from alakazam import _1, _2, _3

import sys

# BGR because OpenCV is funny.
TRANSLATION_TABLE = {
    b'0' : (0, 0, 0),
    b'1' : (1, 0, 0),
    b'2' : (2, 0, 0),
    b'3' : (3, 0, 0),
    b'4' : (4, 0, 0),
    b'5' : (5, 0, 0),
    b'6' : (6, 0, 0),
    b'7' : (7, 0, 0),
    b'8' : (8, 0, 0),
    b'9' : (9, 0, 0),
    b'a' : (0, 1, 0),
    b'b' : (1, 1, 0),
    b'c' : (2, 1, 0),
    b'd' : (3, 1, 0),
    b'e' : (4, 1, 0),
    b'f' : (5, 1, 0),
    b'>' : (0, 5, 0),
    b'^' : (1, 5, 0),
    b'v' : (2, 5, 0),
    b'<' : (3, 5, 0),
    b'_' : (4, 5, 0),
    b'|' : (5, 5, 0),
    b'[' : (6, 5, 0),
    b']' : (7, 5, 0),
    b'x' : (8, 5, 0),
    b'w' : (9, 5, 0),
    b'?' : (0, 6, 0),
    b',' : (0, 0, 1),
    b'.' : (1, 0, 1),
    b'&' : (2, 0, 1),
    b'~' : (3, 0, 1),
    b'+' : (0, 5, 1),
    b'-' : (1, 5, 1),
    b'*' : (2, 5, 1),
    b'/' : (3, 5, 1),
    b'%' : (4, 5, 1),
    b':' : (5, 7, 1),
    b'\\': (6, 7, 1),
    b'#' : (0, 0, 2),
    b'j' : (1, 0, 2),
    b'k' : (2, 0, 2),
    b'"' : (6, 5, 2),
    b'g' : (0, 0, 3),
    b'p' : (1, 0, 3),
    b'\'': (2, 0, 3),
    b's' : (3, 0, 3),
    b'!' : (0, 5, 3),
    b'`' : (1, 5, 3),
    b'{' : (0, 0, 4),
    b'}' : (1, 0, 4),
    b'u' : (2, 0, 4),
    b'n' : (0, 0, 5),
    b'$' : (1, 0, 5),
    b'z' : (4, 5, 5),
    b' ' : (5, 5, 5),
    b'q' : (8, 9, 9),
    b'@' : (9, 9, 9),
}

def lookup(ch):
    return np.array(TRANSLATION_TABLE[ch])
lookup = np.vectorize(lookup, signature='()->(3)')

def pad_right(s, n):
    return s + ' ' * max(0, n - len(s))

image_filename = sys.argv[1]
code_filename = sys.argv[2]

with open(code_filename) as f:
    file_data = []
    for line in f:
        line = line.rstrip("\n")
        file_data.append(line)

max_len = zz.of(file_data).map(len).max()
befunge_array = (zz.of(file_data)
                   .map(lambda s: pad_right(s, max_len))
                   .map(list)
                   .list())
befunge_array = np.array(befunge_array, dtype=np.string_)
befunge_array = lookup(befunge_array)

image = cv.imread(image_filename)

zero_data = np.zeros_like(image)
zero_data += 5
zero_data[:befunge_array.shape[0], :befunge_array.shape[1], :] = befunge_array
befunge_array = zero_data

# Clear the image's last digit data
image = image - image % 10
# Then add in the Befunge data
image += befunge_array

cv.imwrite(image_filename, image)

