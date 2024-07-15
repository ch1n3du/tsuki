(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32)
    i32.const 42
    i32.const 32
    i32.const 12
    f32.const 0x1.71999ap+4 (;=23.1;)
    i32.const 1
    if (result i32)  ;; label = @1
      local.get 2
      local.set 5
      local.get 5
    else
      i32.const 0
      if (result i32)  ;; label = @2
        local.get 2
        local.set 3
        local.get 3
      else
        local.get 2
        local.set 4
        local.get 4
      end
    end
    local.set 1
    drop
    drop
    drop
    drop
    local.get 1)
  (export "add_one" (func 0)))
