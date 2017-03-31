
.386
.MODEL FLAT

.STACK 4096

EXTRN _HeapFree@12:NEAR32
EXTRN _HeapAlloc@12:NEAR32
EXTRN _GetProcessHeap@0:NEAR32
EXTRN _GetStdHandle@4:NEAR32
EXTRN _ExitProcess@4:NEAR32
EXTRN _WriteConsoleA@20:NEAR32

.DATA

    prime_table DWORD 0
    stdout DWORD 0
    written DWORD 0
    arr BYTE "987654321"
    arr_end BYTE 0Dh, 0Ah, 0

.CODE

writeln MACRO ; Destroys eax, ecx, edx; put buffer in eax
    local _loop, _end
    mov ecx, eax

_loop:
    cmp BYTE PTR [ecx], 0
    je _end
    inc ecx
    jmp _loop

_end:
    sub ecx, eax
    mov edx, stdout

    pushd 0
    push OFFSET written
    push ecx
    push eax
    push edx
    call _WriteConsoleA@20

ENDM

sieve MACRO ; Destroys eax, ecx, edx
    local aaa, _outer_loop, _inner_loop, _skip, _end

    mov ecx, 2

_outer_loop:
    cmp ecx, 987654321
    ja _end

    push ecx
    call get_bit
    pop ecx

    cmp eax, 0
    jne _skip

    mov eax, ecx

_inner_loop:
    add eax, ecx

    cmp eax, 987654321
    ja _skip

    push ecx
    push eax
    call put_bit
    pop eax
    pop ecx

    jmp _inner_loop

_skip:
    inc ecx
    jmp _outer_loop

_end:

ENDM

_start:
    pushd -11 ; STD_OUTPUT_HANDLE
    call _GetStdHandle@4
    mov stdout, eax

    call _GetProcessHeap@0
    pushd 123456791
    pushd 8 ; HEAP_ZERO_MEMORY
    push eax
    call _HeapAlloc@12
    mov prime_table, eax

    sieve

    lea ebx, arr
main_loop:
    push ebx
    call str_to_int
    add esp, 4

    push eax
    call is_prime
    add esp, 4

    cmp eax, 1
    je success

    push ebx
    call next_perm
    add esp, 4

    cmp eax, 1
    je main_loop

    call reset
    inc ebx

    lea eax, arr_end
    cmp ebx, eax
    jb main_loop

success:
    mov eax, ebx
    writeln

    call _GetProcessHeap@0
    mov ebx, prime_table
    push ebx
    pushd 0
    push eax
    call _HeapFree@12

    pushd 0
    call _ExitProcess@4

;; All of these procedures are cdecl, unless explicitly stated otherwise

    ;; void reset();
reset PROC
    pushfd

    mov cl, '9'
    lea eax, arr

reset_loop:
    mov [eax], cl
    dec cl
    inc eax
    cmp cl, '0'
    jne reset_loop

    popfd
    ret
reset ENDP

    ;; uint32_t next_perm(uint32_t start); // Returns whether there was a next permutation
next_perm PROC
    push ebp
    mov ebp, esp
    pushfd
    push ebx

    ;; Initialize
    mov eax, 0
    mov ebx, [ebp + 8]
    lea ecx, arr_end
    sub ecx, 2

    ;; Identify pivot
next_perm_pivot:
    cmp ecx, ebx
    jb next_perm_end
    mov dl, [ecx]
    mov dh, [ecx + 1]
    cmp dl, dh
    ja next_perm_has_pivot
    dec ecx
    jmp next_perm_pivot

next_perm_has_pivot:
    lea eax, arr_end
    dec eax

    ;; Identify swap point
next_perm_locate_swap:
    mov dh, [eax]
    cmp dl, dh
    ja next_perm_locate_has_swap
    dec eax
    jmp next_perm_locate_swap

next_perm_locate_has_swap:
    xchg dl, [eax]
    xchg dh, [ecx]

    ;; Reverse the suffix
    lea eax, arr_end

next_perm_rev_loop:
    dec eax
    inc ecx
    cmp ecx, eax
    jae next_perm_end_loop
    mov dl, [eax]
    xchg dl, [ecx]
    mov [eax], dl

next_perm_end_loop:
    mov eax, 1

next_perm_end:
    pop ebx
    popfd
    pop ebp
    ret

next_perm ENDP

    ;; uint32_t get_bit(uint32_t n);
get_bit PROC

    push ebp
    mov ebp, esp
    push ebx
    pushfd

    mov eax, [ebp + 8]
    mov ebx, prime_table
    mov edx, 0
    mov ecx, 8
    div ecx

    add ebx, eax
    mov al, [ebx]

    mov cl, dl
    shr al, cl
    and eax, 1

    popfd
    pop ebx
    pop ebp
    ret

get_bit ENDP

    ;; void put_bit(uint32_t n);
put_bit PROC

    push ebp
    mov ebp, esp
    push ebx
    pushfd

    mov eax, [ebp + 8]
    mov ebx, prime_table
    mov edx, 0
    mov ecx, 8
    div ecx

    add ebx, eax
    mov al, [ebx]

    mov cl, dl
    mov edx, 1
    shl dl, cl
    or al, dl

    mov [ebx], al

    popfd
    pop ebx
    pop ebp
    ret

put_bit ENDP

    ;; uint32_t is_prime(uint32_t n);
is_prime PROC

    push ebp
    mov ebp, esp
    pushfd

    mov eax, [ebp + 8]
    push eax
    call get_bit
    add esp, 4

    xor eax, 1

    popfd
    pop ebp
    ret

is_prime ENDP

    ;; uint32_t str_to_int(uint32_t ptr);
str_to_int PROC

    push ebp
    mov ebp, esp
    pushfd

    mov eax, 0
    mov ecx, [ebp + 8]

sti_loop:
    mov dl, 0Dh
    cmp [ecx], dl
    je sti_end

    mov edx, 10
    mul edx

    mov dl, [ecx]
    sub dl, '0'
    movzx edx, dl
    add eax, edx

    inc ecx
    jmp sti_loop

sti_end:
    popfd
    pop ebp
    ret

str_to_int ENDP

PUBLIC _start
END _start
