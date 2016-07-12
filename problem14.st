
Smalltalk at: #sto put: Dictionary new.

Object subclass: Calc [
    Calc >> hailstone: n [
        ^ n == 1
          ifTrue: [1]
          ifFalse: [((Smalltalk at: #sto) at: n ifAbsent: [ |result|
              result := 1 + (self hailstone: (n even ifTrue: [n / 2] ifFalse: [(n * 3) + 1])).
              (Smalltalk at: #sto) at: n put: result.
              ^result
          ])]
   ]

]

calc := Calc new.

maximal := 0.
maximalN := 0.
temp := 0.

1000000 to: 1 by: -1 do: [:n |
  temp := calc hailstone: n.
  temp > maximalN ifTrue: [ maximalN := temp. maximal := n. ].
].

Transcript show: maximal printString.
