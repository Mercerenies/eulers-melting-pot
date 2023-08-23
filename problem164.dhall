
let Prelude =
  https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:397ef8d5cf55e576eab4359898f61a4e50058982aaace86268c62418d3027871

let Input: Type = { a : Natural, b : Natural }

-- List of possible a and b values, which we'll iterate over frequently.
let possibleInputs =
  Prelude.List.concatMap Natural Input
    (λ(a : Natural) →
      Prelude.List.map Natural Input
        (λ(b : Natural) → { a, b })
        (Prelude.Natural.enumerate 10))
    (Prelude.Natural.enumerate 10)

let nth =
  λ(n : Natural) → λ(xs : List Natural) →
    Prelude.Optional.default Natural 0 (Prelude.List.index n Natural xs)

let calculateCell =
  λ(lastRow : List Natural) → λ(input : Input) →
    let cLimit = Natural/subtract input.b (Natural/subtract input.a 10) in
    Prelude.Natural.sum
      (Prelude.List.map Natural Natural
        (λ(c : Natural) → nth (input.b * 10 + c) lastRow)
        (Prelude.Natural.enumerate cLimit))

let nextList =
  λ(lastRow : List Natural) →
    Prelude.List.map Input Natural (calculateCell lastRow) possibleInputs

let initialList = Prelude.List.replicate 100 Natural 1

let finalList = Natural/fold 19 (List Natural) nextList initialList

let solution =
  Prelude.Natural.sum
    (Prelude.List.map Natural Natural (λ(index : Natural) → nth index finalList) [1, 2, 3, 4, 5, 6, 7, 8, 9])

in { solution }
