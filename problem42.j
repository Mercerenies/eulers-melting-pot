
.class public problem42
.super java/lang/Object

.method public <init>()V

   aload_0
   invokespecial java/lang/Object/<init>()V
   return

.end method

.method public static isTriangular(I)Z

    .limit stack 4
    .limit locals 1

    ldc 1

loop:

    dup
    dup
    ldc 1
    iadd
    imul
    ldc 2
    idiv

    dup
    iload 0
    if_icmpne next1

    pop2
    ldc 1
    ireturn

next1:

    iload 0
    if_icmple next2

    pop
    ldc 0
    ireturn

next2:
    ldc 1
    iadd

    goto loop

.end method

.method public static main([Ljava/lang/String;)V

    .limit stack 4
    .limit locals 6 ; args, arr, counter, i, curr, currx

    new java/io/File
    dup

    ldc "./files/p042_words.txt"
    invokespecial java/io/File/<init>(Ljava/lang/String;)V

    new java/io/FileReader
    swap
    dup2
    invokespecial java/io/FileReader/<init>(Ljava/io/File;)V
    pop

    new java/io/BufferedReader
    swap
    dup2
    invokespecial java/io/BufferedReader/<init>(Ljava/io/Reader;)V
    pop

    dup
    invokevirtual java/io/BufferedReader/readLine()Ljava/lang/String;
    ldc "\""
    ldc ""
    invokevirtual java/lang/String/replaceAll(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
    ldc ","
    invokevirtual java/lang/String/split(Ljava/lang/String;)[Ljava/lang/String;
    astore 1

    invokevirtual java/io/BufferedReader/close()V

    ldc 0
    dup
    istore 2
    istore 3

xouter:

    aload_1
    iload 3
    aaload
    astore 4

    ldc 0
    istore 5

    ldc 0

xinner:

    dup
    aload 4
    swap

    invokevirtual java/lang/String/charAt(I)C
    ldc 65
    isub
    ldc 1
    iadd

    iload 5
    iadd
    istore 5

    ldc 1
    iadd

    dup
    aload 4
    invokevirtual java/lang/String/length()I
    if_icmplt xinner

    pop

    iload 5
    invokestatic problem42/isTriangular(I)Z

    ifeq skip

    iload 2
    ldc 1
    iadd
    istore 2

skip:

    iload 3
    ldc 1
    iadd
    dup
    istore 3

    aload_1
    arraylength

    if_icmplt xouter

    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 2
    invokevirtual java/io/PrintStream/println(I)V

    return

.end method
