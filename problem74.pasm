
  # Initialize the "cache" (it's not a very good cache since it
  # doesn't actually store anything except the stopping conditions
  # but it suffices for our purposes.
  new P0, "Hash"
  set I0, 0
CACHE_INIT_LOOP:
  set P0[I0], 0
  add I0, 1
  lt I0, 3265921, CACHE_INIT_LOOP # This is 9! * 6 + 1, the biggest possible output
                                  # from the sum of factorials function given a
                                  # six-digit number.

  # Initialize some known cache values (given in the problem)
  set P0[169], 3
  set P0[363601], 3
  set P0[1454], 3
  set P0[871], 2
  set P0[45361], 2
  set P0[872], 2
  set P0[45362], 2

  set I0, 1 # I0 shall store our loop variable
  set I1, 0 # I1 shall store the total count, our final answer
MAIN_LOOP:
  set I8, I0 # I8 shall store the current position in the chain
  set I2, 0 # I2 shall store the current length of the chain
NON_REPEATING_LOOP:
  set I3, P0[I8]
  gt I3, 0, FOUND_IN_CACHE

  # If not found in cache, calculate it
  add I2, 1
  set I4, I8
  set I7, 0
EACH_DIGIT_LOOP:
  mod I5, I4, 10
  div I4, 10
  set I6, 1
FACTORIAL_LOOP:
  eq I5, 0, FACTORIAL_LOOP_END
  mul I6, I5
  sub I5, 1
  branch FACTORIAL_LOOP
FACTORIAL_LOOP_END:
  add I7, I6
  ne I4, 0, EACH_DIGIT_LOOP
  # If I7 == I8, then we've hit a trivial cycle.
  eq I7, I8, TRIVIAL_CYCLE
  # Otherwise, keep going
  set I8, I7
  branch NON_REPEATING_LOOP

TRIVIAL_CYCLE:
  set P0[I8], I2
  branch MAIN_LOOP_CONCLUSION

FOUND_IN_CACHE:
  add I2, I3

MAIN_LOOP_CONCLUSION:
  ne I2, 60, END_OF_MAIN_LOOP_ITER
  add I1, 1

END_OF_MAIN_LOOP_ITER:
  add I0, 1
  lt I0, 1000000, MAIN_LOOP

  print I1
  print "\n"
  end
