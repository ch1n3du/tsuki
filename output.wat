(module
  (type $t0 (func (param i32) (result i32)))
  (func $fib (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32) (local $l3 i32)
    local.get $p0
    i32.const 2
    i32.lt_s
    if $I0 (result i32)
      i32.const 1
      local.set $l2
      local.get $l2
    else
      i32.const 3
      i32.const 2
      i32.add
      local.set $l3
      local.get $l3
    end
    local.set $l1
    local.get $l1)
  (export "fib" (func $fib)))
