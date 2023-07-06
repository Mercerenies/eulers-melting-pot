
REBOL [
    Title: "Project Euler Problem 160"
    Date: 5-Jul-2023
    File: %problem160.r
    Author: "Silvio Mayolo"
    Version: 1.0.0
]

limit: 12'800'000

digit-count: 5
divisor: 100'000

add-to: func [
    series quantity
] [
    change series quantity + first series
]

powermod: func [
    factor count divisor
    /local total
] [
    ; We have to dip into IEEE floats here to avoid overflowing
    ; 32-bit integers, but we don't actually lose any precision.
    total: 1.0
    loop count [
        total: total * factor // divisor
    ]
    to-integer total
]

update-multiples: func [
    multiples power-of-5
    /local last-main-answer main-answers-count
] [
    repeat i divisor - 1 [
        either i // 5 <> 0 [
            last-main-answer: round/to/floor limit / power-of-5 100'000
            main-answers-count: round/floor limit / power-of-5 / divisor
            if (last-main-answer + i) <= (round/floor limit / power-of-5) [
                main-answers-count: main-answers-count + 1
            ]
            multiples: add-to multiples main-answers-count
        ] [
            multiples: next multiples
        ]
    ]
]

count-all-fives: has [total power-of-5] [
    total: 0
    power-of-5: 5
    while [power-of-5 < limit] [
        total: total + round/floor limit / power-of-5
        power-of-5: power-of-5 * 5
    ]
    total
]

multiples: array/initial divisor 0

; First, count every factor (divisor 10^5) that goes into
; our factorial. Explicitly do NOT count the powers of 5.
; Basically, count every factor with its fives divided out
; (divisor 10^5)
power-of-5: 1
while [power-of-5 <= limit] [
    update-multiples multiples power-of-5
    power-of-5: power-of-5 * 5
]

; Now divide out twos from the first several multiples of
; 2. Let N = count-all-fives. We want to remove a single
; power of 2 from each of [2, 4, 6, 8, ... 2N-2, 2N]. But
; remember that these numbers have already had their fives
; removed, so we have to take that into consideration when
; removing values.
values-to-remove: count-all-fives
current-value: 1
while [values-to-remove > 0] [
    old-value: current-value * 2 // divisor
    new-value: current-value // divisor
    add-to at multiples old-value (- round/floor limit / 2 / divisor)
    add-to at multiples new-value (round/floor limit / 2 / divisor)
    values-to-remove: values-to-remove - round/floor limit / 2 / divisor
    current-value: current-value + 1
    if current-value // 5 = 0 [
        current-value: current-value + 1
    ]
]

; If we removed too many values in our wholesale removal,
; then add some back by hand.
current-value: 1
while [values-to-remove < 0] [
    old-value: current-value // divisor
    new-value: current-value * 2 // divisor
    add-to at multiples old-value -1
    add-to at multiples new-value 1
    values-to-remove: values-to-remove + 1
    current-value: current-value + 1
    if current-value // 5 = 0 [
        current-value: current-value + 1
    ]
]

; Now we have all the multiples. Multiply it out.
product: 1
factor: 1
foreach count multiples [
    ; We have to dip into IEEE floats here to avoid overflowing
    ; 32-bit integers, but we don't actually lose any precision.
    product: to-integer (product * to-decimal powermod factor count divisor) // divisor
    factor: factor + 1
]
print product
