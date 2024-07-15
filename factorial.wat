(module
  (type $t0 (func (param i32)))
  (type $t1 (func (param i32) (result i32)))
  (import "env" "log" (func $env.log (type $t0)))
  (func $factorial (type $t1) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32)
    local.get $p0
    local.set $l1
    i32.const 1
    local.set $l2
    block $B0
      loop $L1
        local.get $l2
        call $env.log
        local.get $l1
        i32.const 0
        i32.eq
        if $I2
          br $B0
        else
          local.get $l1
          local.get $l2
          i32.mul
          local.set $l2
          local.get $l1
          i32.const 1
          i32.sub
          local.set $l1
        end
        br $L1
      end
    end
    local.get $l2)
  (export "factorial" (func $factorial)))
