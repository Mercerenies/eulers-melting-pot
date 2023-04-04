(module
 (type $FUNCSIG$iii (func (param i32 i32) (result i32)))
 (import "system" "putc" (func $putc (param i32 i32) (result i32)))
 (import "env" "stdout" (global $stdout i32))
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "main" (func $main))
 (func $main (; 1 ;) (result i32)
  (drop
   (call $putc
    (i32.const 97)
    (i32.load
     (get_global $stdout)
    )
   )
  )
  (i32.const 0)
 )
)
