
open import Data.List
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Nat
open import Data.Nat.Show using (show)
open import Data.Unit
open import Data.Bool hiding (_<?_)
open import Relation.Nullary
open import Function.Base using (case_of_)
open import IO
import Agda.Builtin.IO as BIO

primes : List ℕ
primes = 17 ∷ 13 ∷ 11 ∷ 7 ∷ 5 ∷ 3 ∷ 2 ∷ []

exps-to-num : List ℕ → ℕ
exps-to-num xs = product (zipWith _^_ primes xs)

-- Whee! Special casing to make the termination checker happy :)
pad : List ℕ → List ℕ
pad [] = 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ []
pad (x₁ ∷ []) = 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ x₁ ∷ []
pad (x₁ ∷ x₂ ∷ []) = 0 ∷ 0 ∷ 0 ∷ 0 ∷ 0 ∷ x₁ ∷ x₂ ∷ []
pad (x₁ ∷ x₂ ∷ x₃ ∷ []) = 0 ∷ 0 ∷ 0 ∷ 0 ∷ x₁ ∷ x₂ ∷ x₃ ∷ []
pad (x₁ ∷ x₂ ∷ x₃ ∷ x₄ ∷ []) = 0 ∷ 0 ∷ 0 ∷ x₁ ∷ x₂ ∷ x₃ ∷ x₄ ∷ []
pad (x₁ ∷ x₂ ∷ x₃ ∷ x₄ ∷ x₅ ∷ []) = 0 ∷ 0 ∷ x₁ ∷ x₂ ∷ x₃ ∷ x₄ ∷ x₅ ∷ []
pad (x₁ ∷ x₂ ∷ x₃ ∷ x₄ ∷ x₅ ∷ x₆ ∷ []) = 0 ∷ x₁ ∷ x₂ ∷ x₃ ∷ x₄ ∷ x₅ ∷ x₆ ∷ []
pad xs = xs

count-solns : List ℕ → ℕ
count-solns xs = product (map (λ x → 2 * x + 1) xs)

minimum : List (Maybe ℕ) → Maybe ℕ
minimum = foldr min nothing
  where min : Maybe ℕ → Maybe ℕ → Maybe ℕ
        min nothing y = y
        min (just x) nothing = just x
        min (just x) (just y) with x <? y
        min (just x) (just y) | yes _ = just x
        min (just x) (just y) | no _ = just y

upto : ℕ → List ℕ
upto zero = []
upto (suc n) = suc n ∷ upto n

{-# NON_TERMINATING #-}
recurse : List ℕ → Maybe ℕ
recurse xs with count-solns xs >? 2000
recurse xs | yes _ = just (exps-to-num (pad xs))
recurse xs | no _ =
  let stop = case xs of
       λ { [] → 1000
         ; (x ∷ _) → x
         } in
  minimum (map (λ x → recurse (x ∷ xs)) (upto stop))

main : BIO.IO ⊤
main = run (putStrLn (case recurse [] of
                       λ { nothing → "Error!"
                         ; (just x) → show x
                         }))
