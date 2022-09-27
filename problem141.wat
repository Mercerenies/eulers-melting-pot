(module
  (import "wasi_unstable" "fd_write"
    (func $fd_write (param i32 i32 i32 i32)
                    (result i32)))

  (memory 1)
  (export "memory" (memory 0))
  (data (i32.const 8) "                    \n") ;; 20 chars = longest an unsigned 64-bit integer can be

  ;; Note: Our "found" numbers are i64s stored in global memory.
  ;; Memory position 36 starts the global array of i64s, terminated by
  ;; a -1 value.

  (func $main (export "_start")
    (local $limit i64)
    (local $total i64)
    (local $a i64)
    (local $b i64)
    (local $c i64)
    (local $n i64)

    ;; Initialize "found numbers" list
    (i64.store (i32.const 36) (i64.const -1))

    ;; Initialize other local variables for our computation
    (local.set $limit (i64.const 1000000000000))
    (local.set $total (i64.const 0))
    (local.set $a (i64.const 2))

    (loop
      (local.set $b (i64.const 1))
      (block
        (loop
          (if (i64.eq (call $gcd (local.get $a) (local.get $b)) (i64.const 1))
            (then
              ;; Break if we've already exceeded the limit
              (local.get $b)
              (i64.mul (local.get $a))
              (i64.mul (local.get $a))
              (i64.mul (local.get $a))
              (local.get $b)
              (i64.mul (local.get $b))
              i64.add
              (i64.ge_s (local.get $limit))
              (br_if 2)
              ;; Loop over c
              (local.set $c (i64.const 1))
              (loop
                (local.get $c)
                (i64.mul (local.get $c))
                (i64.mul (local.get $b))
                (i64.mul (local.get $a))
                (i64.mul (local.get $a))
                (i64.mul (local.get $a))
                (local.get $c)
                (i64.mul (local.get $b))
                (i64.mul (local.get $b))
                i64.add
                (local.set $n)
                (br_if 1 (i64.ge_s (local.get $n) (local.get $limit)))
                (if (call $is_square (f64.convert_i64_s (local.get $n)))
                  (then
                    (if (i32.eq (call $in_found_list (local.get $n)) (i32.const 0))
                      (then
                        (call $append_to_found_list (local.get $n))
                        (local.set $total (i64.add (local.get $total) (local.get $n)))
                      )
                    )
                  )
                )
                (local.set $c (i64.add (local.get $c) (i64.const 1)))
                (br 0)
              )
            )
          )
          (local.set $b (i64.add (local.get $b) (i64.const 1)))
          (br_if 0 (i64.lt_s (local.get $b) (local.get $a)))
        )
      )
      (local.set $a (i64.add (local.get $a) (i64.const 1)))
      (br_if 0 (i64.lt_s (local.get $a) (i64.const 10000)))
    )

    (call $print (local.get $total))
  )

  (func $print (param $n i64)
    (local $ptr i32)
    (local.set $ptr (i32.const 27)) ;; Last character before newline in our allocated data string
    (loop
      (local.get $ptr)
      (i64.add (i64.rem_s (local.get $n) (i64.const 10)) (i64.const 48))
      i64.store8
      (local.set $n (i64.div_s (local.get $n) (i64.const 10)))
      (local.set $ptr (i32.sub (local.get $ptr) (i32.const 1)))
      (br_if 0 (i64.gt_s (local.get $n) (i64.const 0)))
    )
    (i32.store (i32.const 0) (i32.add (local.get $ptr) (i32.const 1))) ;; position of our string = ptr + 1
    (i32.store (i32.const 4) (i32.sub (i32.const 28) (local.get $ptr))) ;; length of string = 28 - ptr
    (call $fd_write
      (i32.const 1)  ;; stdout
      (i32.const 0)  ;; array position
      (i32.const 1)  ;; number of strings to print
      (i32.const 32) ;; output (# of bytes written, we don't use this)
    )
    drop
  )

  (func $append_to_found_list (param $n i64)
    (local $ptr i32)
    (local.set $ptr (i32.const 28))
    (loop
      (local.set $ptr (i32.add (local.get $ptr) (i32.const 8)))
      (br_if 0 (i64.ne (i64.load (local.get $ptr)) (i64.const -1)))
    )
    (i64.store (local.get $ptr) (local.get $n))
    (i64.store (i32.add (local.get $ptr) (i32.const 8)) (i64.const -1))
  )

  (func $in_found_list (param $n i64) (result i32)
    (local $ptr i32)
    (local.set $ptr (i32.const 28))
    (loop
      (local.set $ptr (i32.add (local.get $ptr) (i32.const 8)))
      (if (i64.eq (i64.load (local.get $ptr)) (local.get $n))
        (return (i32.const 1))
      )
      (br_if 0 (i64.ne (i64.load (local.get $ptr)) (i64.const -1)))
    )
    (return (i32.const 0))
  )

  (func $is_square (param $x f64) (result i32)
    (local $y f64)
    (local.set $y (f64.sqrt (local.get $x)))
    (f64.eq (local.get $y) (f64.floor (local.get $y)))
  )

  (func $gcd (param $a i64) (param $b i64) (result i64)
    (block
      (loop
        (br_if 1 (i64.eq (local.get $b) (i64.const 0)))
        (local.get $b)
        (i64.rem_s (local.get $a) (local.get $b))
        (local.set $b)
        (local.set $a)
        (br 0)
      )
    )
    (local.get $a)
  )

)
