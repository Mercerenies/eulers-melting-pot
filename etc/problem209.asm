
ORG     $1000
;; The interpreter I'm using has a broken ADD.L instruction, so we'll
;; be storing all of these numbers as *triple* words, across three
;; registers. Thus, I need 64 triple words, or 384 bytes.
FIBO:   ds.b 384

DD0:    ds.w 1
DD1:    ds.w 1
DD2:    ds.w 1
DD3:    ds.w 1
DD4:    ds.w 1
DD5:    ds.w 1

DD05:   ds.w 2
DD15:   ds.w 2
DD25:   ds.w 2
DD14:   ds.w 2
DD24:   ds.w 2
DD23:   ds.w 2

MAIN:
  ;; Initialize Fibonacci Sequence (current fibo values are d0;d1;d2 and d3;d4;d5)
  MOVE.W #0, d0
  MOVE.W #0, d1
  MOVE.W #1, d2
  MOVE.W #0, d3
  MOVE.W #0, d4
  MOVE.W #2, d5
  LEA.L FIBO, a0

  MOVE.W #64, d6

FIBO_LOOP:
  MOVE.W d0, (a0)+
  MOVE.W d1, (a0)+
  MOVE.W d2, (a0)+

  ADD.W d5, d2
  ADDX.W d4, d1
  ADDX.W d3, d0
  EXG.W d0, d3
  EXG.W d1, d4
  EXG.W d2, d5

  SUBQ.L #1, d6
  BNE FIBO_LOOP

  ;; Fibonacci is initialized.
  ;;
  ;; For main loop:
  ;;
  ;; d0;d1;d2 - Sum Total
  ;; d6 - Loop Counter (i)
  ;; d7 - k
  ;; a0 - cycle_length
  MOVE.W #0, d6
  MOVE.W #0, d0
  MOVE.W #0, d1
  MOVE.W #1, d2
MAIN_LOOP:
  MOVEA.W #0, a0    ; cycle_length = 0
  MOVE.W d6, d7     ; k = i
K_LOOP:
  ADDQ.W #1, a0     ; cycle_length += 1
  ;; Use d3, d4, d5 as temporaries (a, b, c)
  MOVE.W d7, d3
  LSR.W #5, d3      ; a = (k >> 5)
  MOVE.W d7, d4
  LSR.W #4, d4      ; b = (k >> 4)
  MOVE.W d7, d5
  LSR.W #3, d5      ; c = (k >> 3)
  ;; d3 = a, d4 = b, d5 = c, we need a ^ (b & c). We store this result
  ;; in d5.
  AND.W d4, d5
  ADD.W d3, d5
  AND.W #1, d5      ; tmp = (a ^ (b & c)) & 1
  AND.W #$1F, d7    ; k = k & 0x1F
  LSL.W #1, d7      ; k *= 2
  ADD.W d5, d7      ; k += tmp
  CMP.W d7, d6      ; cmp = (k <=> i)
  BHI LOOP_END      ; skip if i < k
  BNE K_LOOP        ; continue if i != k
  CMPA.W #2, a0     ; cmp = (2 <=> cycle_length)
  BEQ THREE_CASE
  BPL FIBO_CASE
  JMP LOOP_END

THREE_CASE:
  ;; total *= 3
  MULU #3, d2
  MULU #3, d1
  MULU #3, d0
  MOVE.W d2, d5
  LSR.L #8, d5
  LSR.L #8, d5
  ADD.W d5, d1
  ADDX.W #0, d2
  MOVE.W d1, d5
  LSR.L #8, d5
  LSR.L #8, d5
  ADD.W d5, d0
  AND.L #$FFFF, d0
  AND.L #$FFFF, d1
  AND.L #$FFFF, d2
  JMP LOOP_END

FIBO_CASE:
  ;; d3;d4;d5 will hold our Fibonacci sum
  LEA.L FIBO, a1
  MOVE.W a0, d7
  MULU #6, d7
  ADDA.W d7, a1
  ;; a1 is now the position of cycle_length in the array.
  SUBQ.W #6, a1
  MOVE.W (a1)+, d3
  MOVE.W (a1)+, d4
  MOVE.W (a1)+, d5
  SUBA.W #12, a1
  ADD.W -(a1), d5
  ;; M68k is documented as supported an address offset as the first
  ;; arg of ADDX, but the interpreter we're using balks at it. So
  ;; indirectly move into a7 and then use a7 (which is supported).
  MOVEA.W -(a1), a7
  ADDX.W a7, d4
  MOVEA.W -(a1), a7
  ADDX.W a7, d3

  ;; Goal: Multiply d0;d1;d2 by d3;d4;d5 and put the result in
  ;; d0;d1;d2. Do not touch d6 but everything else is fair game.

  ;; Step 1: Store everything in memory (very inefficient, don't care)
  LEA.L DD0, a0
  MOVE.W d0, (a0)
  LEA.L DD1, a0
  MOVE.W d1, (a0)
  LEA.L DD2, a0
  MOVE.W d2, (a0)
  LEA.L DD3, a0
  MOVE.W d3, (a0)
  LEA.L DD4, a0
  MOVE.W d4, (a0)
  LEA.L DD5, a0
  MOVE.W d5, (a0)

  ;; Step 2: Do each of the multiplications

  ;; NOTE: Again, M68k supports indirects as the source arg of MULU,
  ;; but our interpreter does not. So we're doing some shenanigans.

  ;; d0*d5
  LEA.L DD0, a0
  MOVE.W (a0), d0
  LEA.L DD5, a0
  MOVE.W (a0), d7
  MULU d7, d0
  LEA.L DD05, a0
  MOVE.L d0, (a0)

  ;; d1*d5
  LEA.L DD1, a0
  MOVE.W (a0), d0
  LEA.L DD5, a0
  MOVE.W (a0), d7
  MULU d7, d0
  LEA.L DD15, a0
  MOVE.L d0, (a0)

  ;; d2*d5
  LEA.L DD2, a0
  MOVE.W (a0), d0
  LEA.L DD5, a0
  MOVE.W (a0), d7
  MULU d7, d0
  LEA.L DD25, a0
  MOVE.L d0, (a0)

  ;; d1*d4
  LEA.L DD1, a0
  MOVE.W (a0), d0
  LEA.L DD4, a0
  MOVE.W (a0), d7
  MULU d7, d0
  LEA.L DD14, a0
  MOVE.L d0, (a0)

  ;; d2*d4
  LEA.L DD2, a0
  MOVE.W (a0), d0
  LEA.L DD4, a0
  MOVE.W (a0), d7
  MULU d7, d0
  LEA.L DD24, a0
  MOVE.L d0, (a0)

  ;; d2*d3
  LEA.L DD2, a0
  MOVE.W (a0), d0
  LEA.L DD3, a0
  MOVE.W (a0), d7
  MULU d7, d0
  LEA.L DD23, a0
  MOVE.L d0, (a0)

  ;; Step 3: Add results together

  ;; d2' = (d2d5)ˡ
  LEA.L DD25, a0
  MOVE.L (a0), d2
  AND.L #$FFFF, d2

  ;; d1' = (d2d5)ʰ + (d1d5)ˡ + (d2d4)ˡ [store final carry in d7]
  MOVE.W #0, d7
  LEA.L DD25, a0
  MOVE.L (a0), d1
  LSR.L #8, d1
  LSR.L #8, d1
  LEA.L DD15, a0
  MOVE.L (a0), d3
  AND.L #$FFFF, d3
  ADD.W d3, d1
  ADDX.W #0, d7
  LEA.L DD24, a0
  MOVE.L (a0), d3
  AND.L #$FFFF, d3
  ADD.W d3, d1
  ADDX.W #0, d7

  ;; d0' = (d1d5)ʰ + (d2d4)ʰ + (d0d5)ˡ + (d1d4)ˡ + (d2d3)ˡ + carry(d1') [carry is stored in d7]
  LEA.L DD15, a0
  MOVE.L (a0), d0
  LSR.L #8, d0
  LSR.L #8, d0
  LEA.L DD24, a0
  MOVE.L (a0), d3
  LSR.L #8, d3
  LSR.L #8, d3
  ADD.W d3, d0
  LEA.L DD05, a0
  MOVE.L (a0), d3
  AND.L #$FFFF, d3
  ADD.W d3, d0
  LEA.L DD14, a0
  MOVE.L (a0), d3
  AND.L #$FFFF, d3
  ADD.W d3, d0
  LEA.L DD23, a0
  MOVE.L (a0), d3
  AND.L #$FFFF, d3
  ADD.W d3, d0
  ADD.W d7, d0

LOOP_END:
  ADDQ.W #1, d6     ; i += 1
  CMP.W #64, d6
  BNE MAIN_LOOP

  ;; Output in d0;d1;d2
END
