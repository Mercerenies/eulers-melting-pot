
ORG     $1000

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

  MOVE.W #$10, d0
  MOVE.W #$12, d1
  MOVE.W #$22, d2
  MOVE.W #$00, d3
  MOVE.W #$A0, d4
  MOVE.W #$A1, d5
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
END
