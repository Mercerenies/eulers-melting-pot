
import data.nat.basic
import algebra.parity

variables {α : Type*}

def clamp [linear_order α] (x a b: α): α :=
  linear_order.min (linear_order.max x a) b

def sum_zero_up_to [has_zero α] [has_add α] (limit: nat) (f: nat → α): α :=
  nat.rec 0 (λ n acc, f n + acc) limit

-- Note: We re-order some terms from the Rust code so that the monus
-- operator doesn't lose information. Specifically, we always do
-- subtraction last, after adding everything together.
def count_dia_rectangles_at (w h x y: nat): nat :=
  if (x + y) % 2 = 1 then
    0
  else
    let upper := linear_order.min y (w - x) in
    let mid_bound := clamp (1 + w + y - x - h) 1 (1 + upper) in
    (h - y) * (mid_bound - 1) + (w - x) * (1 + upper - mid_bound) - (upper * (1 + upper) - mid_bound * (mid_bound - 1)) / 2

def count_dia_rectangles_y (w h x: nat): nat :=
  sum_zero_up_to h.succ (count_dia_rectangles_at w h x)

def count_dia_rectangles (w h: nat): nat :=
  sum_zero_up_to w.succ (count_dia_rectangles_y w h)

def count_aa_rectangles (w h: nat): nat :=
  (w * w + w) * (h * h + h) / 4

def count_total_rectangles (w h: nat): nat :=
  count_dia_rectangles (2 * w) (2 * h) + count_aa_rectangles w h

-- Returns 0 if either arg is 0.
def count_total_rectangles_checked (w h: nat): nat :=
  if w = 0 ∨ h = 0 then
    0
  else
    count_total_rectangles w h

def count_all_given_i (i: nat): nat :=
  sum_zero_up_to 44 (count_total_rectangles_checked i)

def count_all: nat :=
  sum_zero_up_to 48 count_all_given_i

#eval count_all
