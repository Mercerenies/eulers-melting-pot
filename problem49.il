
.assembly Problem49 {}
.assembly extern mscorlib {}

.method static void Sieve(int8[]) {
    .maxstack 4
    .locals init (int32,int8[],int32)

    ldarg.0
    stloc.1

    ldc.i4.1
L0: ldc.i4.1
    add
    dup
    stloc.0
    ldloc.1
    ldloc.0
    ldelem.i1
    brtrue L2
L1: ldloc.0
    add
    dup
    ldc.i4 9999
    bgt L2
    dup
    stloc.2
    ldloc.1
    ldloc.2
    ldc.i4.1
    stelem.i1
    br.s L1
L2: pop
    ldloc.0
    dup
    ldc.i4 9999
    blt L0

    pop
    ret
}

// To avoid IndexOutOfRange, returns false if value is greater than 9999
.method static bool IsPrime(int8[],int32) {
    .maxstack 3

    ldarg.1
    ldc.i4 10000
    blt L0

    ldc.i4.0
    ret

L0: ldc.i4.1
    ldarg.0
    ldarg.1
    ldelem.i1
    sub

    ret
}

.method static bool IsPerm(int32,int32) {
    .maxstack 3
    .locals init (int8[],int8)

    ldc.i4 10
    newarr int8
    stloc.0

    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    add
    stloc.1
    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.0
    ldc.i4 10
    div
    starg.s 0

    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    add
    stloc.1
    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.0
    ldc.i4 10
    div
    starg.s 0

    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    add
    stloc.1
    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.0
    ldc.i4 10
    div
    starg.s 0

    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    add
    stloc.1
    ldloc.0
    ldarg.0
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.0
    ldc.i4 10
    div
    starg.s 0

    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    sub
    stloc.1
    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.1
    ldc.i4 10
    div
    starg.s 1

    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    sub
    stloc.1
    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.1
    ldc.i4 10
    div
    starg.s 1

    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    sub
    stloc.1
    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.1
    ldc.i4 10
    div
    starg.s 1

    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldelem.i1
    ldc.i4.1
    sub
    stloc.1
    ldloc.0
    ldarg.1
    ldc.i4 10
    rem
    ldloc.1
    stelem.i1
    ldarg.1
    ldc.i4 10
    div
    starg.s 1

    ldc.i4.0
L0: stloc.1
    ldloc.0
    ldloc.1
    ldelem.i1
    brtrue L1
    ldloc.1
    ldc.i4.1
    add
    dup
    ldc.i4 10
    blt L0

    pop
    ldc.i4.1
    ret

L1: ldc.i4.0
    ret
}

.method static void Main() {
    .entrypoint
    .maxstack 3
    .locals init (int8[],int32,int32,int32)

    ldc.i4 10000
    newarr int8
    dup
    stloc.0
    call void Sieve(int8[])

    ldc.i4 1000
    stloc.1
L0: ldloc.1
    ldc.i4.1
    add
    stloc.2
L1: ldloc.2
    ldc.i4.2
    mul
    ldloc.1
    sub
    stloc.3

    ldloc.0
    ldloc.1
    call bool IsPrime(int8[],int32)
    brfalse L2

    ldloc.0
    ldloc.2
    call bool IsPrime(int8[],int32)
    brfalse L2

    ldloc.0
    ldloc.3
    call bool IsPrime(int8[],int32)
    brfalse L2

    ldloc.1
    ldloc.2
    call bool IsPerm(int32,int32)
    brfalse L2

    ldloc.1
    ldloc.3
    call bool IsPerm(int32,int32)
    brfalse L2

    ldloc.1
    call void [mscorlib]System.Console::Write(int32)
    ldloc.2
    call void [mscorlib]System.Console::Write(int32)
    ldloc.3
    call void [mscorlib]System.Console::Write(int32)
    ldstr ""
    call void [mscorlib]System.Console::WriteLine(string)
    br.s L4

L2: ldloc.2
    ldc.i4.1
    add
    dup
    stloc.2
    ldc.i4 9999
    blt L1

    ldloc.1
    ldc.i4.1
    add
    dup
    ldc.i4 1487
    bne.un L3
    ldc.i4.1
    add
L3: dup
    stloc.1
    ldc.i4 9999
    blt L0

L4: ret
}
