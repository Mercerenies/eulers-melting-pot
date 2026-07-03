
ORG     $1000
;; The interpreter I'm using has a broken ADD.L instruction, so we'll
;; be storing all of these numbers as *triple* words, across three
;; registers. Thus, I need 64 triple words, or 384 bytes.
FIBO:   ds.b 384

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
  MOVE.W #0, d2
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
  BCS LOOP_END      ; skip if i < k
  BNE K_LOOP        ; continue if i != k
  CMPA.W #2, a0     ; cmp = (2 <=> cycle_length)
  BEQ THREE
  BHI FIBO
  JMP LOOP_END

THREE:
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

FIBO:
  ;; d3;d4;d5 will hold our Fibonacci sum
  LEA.L FIBO, a1
  MOVE.W #5, d7
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
  JMP LOOP_END

LOOP_END:
  ADDQ.W #1, d6     ; i += 1
  CMP.W #64, d6
  JNE MAIN_LOOP

  ;; Output in d0;d1;d2
END
