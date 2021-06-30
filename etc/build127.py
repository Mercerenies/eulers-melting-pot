
# (Note: This is for a draft Pickle implementation of 127 that ended
# up not working out. This does NOT build the final Nit solution.)

# Trivial Python wrapper to load the pickle for Problem 127. Run from
# project root directory.

import pickle
import sys

sys.setrecursionlimit(5000)
with open("./problem127.pickle", "rb") as f:
    pickle.load(f)
