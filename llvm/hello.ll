; ModuleID = 'hello.ll'
source_filename = "hello.ll"

@.str = private unnamed_addr constant [14 x i8] c"Hello, World!\00", align 1

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
    ; Get the address of the string
    %str_ptr = getelementptr inbounds [14 x i8], [14 x i8]* @.str, i32 0, i32 0

    ; Call printf to print the string
    %call = call i32 (i8*, ...) @printf(i8* %str_ptr)

    ; Return 0
    ret i32 0
}

