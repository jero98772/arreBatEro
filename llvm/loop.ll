; ModuleID = 'my_module'
source_filename = "my_module"

define i32 @sum(i32 %a, i32 %b) {
entry:
  %sum = alloca i32, align 4
  store i32 %a, i32* %sum, align 4
  br label %loop

loop:                                             ; preds = %loop, %entry
  %sumval = load i32, i32* %sum, align 4
  %newsum = add i32 %sumval, %b
  store i32 %newsum, i32* %sum, align 4
  %loopcond = icmp slt i32 %newsum, 100
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %loop
  %finalsum = load i32, i32* %sum, align 4
  ret i32 %finalsum
}
