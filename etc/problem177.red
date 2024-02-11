
Red [
    Title: "Project Euler Problem 177"
    Date: 10-Feb-2024
    File: %problem177.red
    Author: "Silvio Mayolo"
    version: 1.0.0
]

clamp: func [
    value [integer!]
    lower [integer!]
    upper [integer!]
] [
    min upper max lower value
]

; Memoize all trig function values we'll need in advance.
sines: make block! 180
cosines: make block! 180
repeat i 179 [
  append sines sine i
  append cosines cosine i
]

print clamp -1 0 3