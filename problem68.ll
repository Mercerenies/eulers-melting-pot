
@perm = internal global [10 x i64] zeroinitializer, align 8
@final = internal global i64 0, align 8
@printfstring = internal constant [6 x i8] c"%lld\0A\00"

declare i32 @printf(i8*, ...)

define i64 @value_of(i64 %p, i64 %index) {
entry:
  %ptr = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 %index
  %valueat = load i64, i64* %ptr
  %cmp = icmp eq i64 %valueat, 10
  br i1 %cmp, label %ten, label %default

ten:
  %v1 = mul i64 %p, 100
  %v2 = add i64 %v1, 10
  ret i64 %v2

default:
  %v3 = mul i64 %p, 10
  %v4 = add i64 %v3, %valueat
  ret i64 %v4

}

define void @validate() {
entry:
  ;; I need random access to the array in really bizarre orders, so
	;; we'll just get them all here.
  %pp0 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 0
  %pp1 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 1
  %pp2 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 2
  %pp3 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 3
  %pp4 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 4
  %pp5 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 5
  %pp6 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 6
  %pp7 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 7
  %pp8 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 8
  %pp9 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 9
  %p0 = load i64, i64* %pp0
  %p1 = load i64, i64* %pp1
  %p2 = load i64, i64* %pp2
  %p3 = load i64, i64* %pp3
  %p4 = load i64, i64* %pp4
  %p5 = load i64, i64* %pp5
  %p6 = load i64, i64* %pp6
  %p7 = load i64, i64* %pp7
  %p8 = load i64, i64* %pp8
  %p9 = load i64, i64* %pp9

  %c1 = icmp sgt i64 %p0, %p1
  %c2 = icmp sgt i64 %p0, %p2
  %c3 = icmp sgt i64 %p0, %p3
  %c4 = icmp sgt i64 %p0, %p4
  %cc0 = or i1 %c1, %c2
  %cc1 = or i1 %c3, %c4
  %cc2 = or i1 %cc0, %cc1

  %total0 = add i64 %p0, %p5
  %total = add i64 %total0, %p6

  %t00 = add i64 %p1, %p6
  %t01 = add i64 %t00, %p7

  %t10 = add i64 %p2, %p7
  %t11 = add i64 %t10, %p8

  %t20 = add i64 %p3, %p8
  %t21 = add i64 %t20, %p9

  %t30 = add i64 %p4, %p9
  %t31 = add i64 %t30, %p5

  %x1 = icmp ne i64 %total, %t01
  %x2 = icmp ne i64 %total, %t11
  %x3 = icmp ne i64 %total, %t21
  %x4 = icmp ne i64 %total, %t31

  %xx0 = or i1 %x1, %x2
  %xx1 = or i1 %x3, %x4
  %xx2 = or i1 %xx0, %xx1

  %valid = or i1 %cc2, %xx2
  br i1 %valid, label %abort, label %proceed

abort:
  ret void

proceed:
  %value0 = call i64 @value_of(i64 0, i64 0)
  %value1 = call i64 @value_of(i64 %value0, i64 5)
  %value2 = call i64 @value_of(i64 %value1, i64 6)
  %value3 = call i64 @value_of(i64 %value2, i64 1)
  %value4 = call i64 @value_of(i64 %value3, i64 6)
  %value5 = call i64 @value_of(i64 %value4, i64 7)
  %value6 = call i64 @value_of(i64 %value5, i64 2)
  %value7 = call i64 @value_of(i64 %value6, i64 7)
  %value8 = call i64 @value_of(i64 %value7, i64 8)
  %value9 = call i64 @value_of(i64 %value8, i64 3)
  %value10 = call i64 @value_of(i64 %value9, i64 8)
  %value11 = call i64 @value_of(i64 %value10, i64 9)
  %value12 = call i64 @value_of(i64 %value11, i64 4)
  %value13 = call i64 @value_of(i64 %value12, i64 9)
  %value14 = call i64 @value_of(i64 %value13, i64 5)

  %finalval = load i64, i64* @final
  %d0 = icmp sgt i64 %value14, %finalval
  %d1 = icmp slt i64 %value14, 10000000000000000
  %d2 = and i1 %d0, %d1
  br i1 %d2, label %replace, label %abort

replace:
  store i64 %value14, i64* @final
  ret void

}

define void @permute(i64 %n) {
entry:
  %exitcond = icmp eq i64 %n, 1
  br i1 %exitcond, label %exitrec, label %baseloop

exitrec:
  call void @validate()
  ret void

baseloop:
  %counter = phi i64 [0, %entry], [%counter1, %nextiter]
  %n1 = sub i64 %n, 1
  call void @permute(i64 %n1)
  %parity = srem i64 %n, 2
  %paritycmp = icmp eq i64 %parity, 1
  br i1 %paritycmp, label %odd, label %even

odd:
  %ptr1 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 0
  %ptr2 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 %n1
  %v1 = load i64, i64* %ptr1
  %v2 = load i64, i64* %ptr2
  store i64 %v1, i64* %ptr2
  store i64 %v2, i64* %ptr1
  br label %nextiter

even:
  %ptr3 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 %counter
  %ptr4 = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 %n1
  %v3 = load i64, i64* %ptr3
  %v4 = load i64, i64* %ptr4
  store i64 %v3, i64* %ptr4
  store i64 %v4, i64* %ptr3
  br label %nextiter

nextiter:
  %counter1 = add i64 %counter, 1
  %countercmp = icmp sge i64 %counter1, %n
  br i1 %countercmp, label %exitloop, label %baseloop

exitloop:
  ret void

}

define void @main() {
entry:
  br label %initloop

initloop:
  %counter = phi i64 [0, %entry], [%counter1, %initloop]
  %initlooppos = getelementptr [10 x i64], [10 x i64]* @perm, i64 0, i64 %counter
  %counter1 = add i64 %counter, 1
  store i64 %counter1, i64* %initlooppos
  %cmptmp = icmp sge i64 %counter1, 10
  br i1 %cmptmp, label %primary, label %initloop

primary:
  call void @permute(i64 10)
  %result = load i64, i64* @final
  %format = getelementptr [6 x i8], [6 x i8]* @printfstring, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %format, i64 %result)
  ret void
}
