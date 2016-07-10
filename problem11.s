

BITS 64

GLOBAL main
EXTERN puts

SECTION .data
array:  db 8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8, 49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0, 81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65, 52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91, 22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80, 24, 47, 32, 60, 99, 3, 45, 2, 44, 75,  3, 53, 78, 36, 84, 20, 35, 17, 12, 50, 32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70, 67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21, 24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72, 21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95, 78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92, 16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57, 86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58, 19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40, 4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66, 88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69, 4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36, 20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16, 20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54, 1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48
string: db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

SECTION .text
        ;; int16_t get_pos(int16_t row, int16_t col)
get_pos:
        push rbp
        mov rbp, rsp

        push rbx
        push rdx
        pushfq

        mov rbx, array
        mov ax, [rbp + 16]
        mov dx, 20
        mul dx
        mov dx, [rbp + 18]
        add ax, dx
        movzx rax, ax
        add rbx, rax
        mov al, [rbx]
        movzx rax, al

        popfq
        pop rdx
        pop rbx

        pop rbp
        ret

        ;; int16_t get_pos_safe(int16_t row, int16_t col)
        ;; Returns 0 if out of bounds
get_pos_safe:
        push rbp
        mov rbp, rsp

        pushfq

        mov ax, [rbp + 16]
        cmp ax, 0
        jl _oob
        cmp ax, 20
        jge _oob
        mov ax, [rbp + 18]
        cmp ax, 0
        jl _oob
        cmp ax, 20
        jge _oob

        mov ax, [rbp + 18]
        push ax
        mov ax, [rbp + 16]
        push ax
        call get_pos
        add rsp, 4
        jmp _safe_rnd

_oob:
        mov rax, 0
_safe_rnd:

        popfq
        pop rbp
        ret

        ;; int64_t do_prod(int16_t x0, int16_t x1, int16_t x2, int16_t x3)
do_prod:
        push rbp
        mov rbp, rsp

        push rdx
        pushfq

        mov rax, 1
        mov rdx, 0
        mov dx, [rbp + 16]
        mul rdx
        mov rdx, 0
        mov dx, [rbp + 18]
        mul rdx
        mov rdx, 0
        mov dx, [rbp + 20]
        mul rdx
        mov rdx, 0
        mov dx, [rbp + 22]
        mul rdx

        popfq
        pop rdx

        pop rbp
        ret

        ;; int64_t dir_up(int16_t row, int16_t col)
dir_up:
        push rbp
        mov rbp, rsp

        push rbx
        push rcx
        pushfq

        mov bx, [rbp + 16] ; Row
        mov cx, [rbp + 18] ; Column

        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec bx

        call do_prod
        add rsp, 8

        popfq
        pop rcx
        pop rbx

        pop rbp
        ret

        ;; int64_t dir_left(int16_t row, int16_t col)
dir_left:
        push rbp
        mov rbp, rsp

        push rbx
        push rcx
        pushfq

        mov bx, [rbp + 16] ; Row
        mov cx, [rbp + 18] ; Column

        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx

        call do_prod
        add rsp, 8

        popfq
        pop rcx
        pop rbx

        pop rbp
        ret

        ;; int64_t dir_dia1(int16_t row, int16_t col)
dir_dia1:
        push rbp
        mov rbp, rsp

        push rbx
        push rcx
        pushfq

        mov bx, [rbp + 16] ; Row
        mov cx, [rbp + 18] ; Column

        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        dec bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        dec bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        dec bx

        call do_prod
        add rsp, 8

        popfq
        pop rcx
        pop rbx

        pop rbp
        ret

        ;; int64_t dir_dia2(int16_t row, int16_t col)
dir_dia2:
        push rbp
        mov rbp, rsp

        push rbx
        push rcx
        pushfq

        mov bx, [rbp + 16] ; Row
        mov cx, [rbp + 18] ; Column

        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        inc bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        inc bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        inc bx
        push cx
        push bx
        call get_pos_safe
        add rsp, 4
        push ax
        dec cx
        inc bx

        call do_prod
        add rsp, 8

        popfq
        pop rcx
        pop rbx

        pop rbp
        ret

        ;; void ltostr(int64_t x0)
        ;; Stores result in 'string'
ltostr:
        push rbp
        mov rbp, rsp

        push rax
        push rbx
        push rcx
        push rdx
        push rsi
        push rdi
        pushfq

        mov rax, string
        mov rdi, rax
        mov rsi, rax
        mov rax, [rbp + 16]

_lto_loop:
        mov rdx, 0
        mov rcx, 10
        div rcx
        ;; 0x30 == '0'
        add rdx, 0x30
        mov [rdi], dl
        inc rdi
        cmp rax, 0
        jnz _lto_loop

        mov byte [rdi], 0

        ;; We want to reverse the string
_lto_rev_loop:
        dec rdi
        mov al, [rdi]
        xchg al, [rsi]
        mov [rdi], al
        inc rsi
        cmp rsi, rdi
        jb _lto_rev_loop

        popfq
        pop rdi
        pop rsi
        pop rdx
        pop rcx
        pop rbx
        pop rax

        pop rbp
        ret

        ;; int64_t max(int64_t n0, int64_t n1)
max:
        push rbp
        mov rbp, rsp

        push rbx
        push rcx
        pushfq

        mov rbx, [rbp + 24]
        mov rcx, [rbp + 16]
        cmp rbx, rcx
        ja _first_g

        mov rax, rcx
        jmp _second_g

_first_g:
        mov rax, rbx

_second_g:

        popfq
        pop rcx
        pop rbx

        pop rbp
        ret

main:
        push rbp
        mov rbp, rsp

        mov rax, 0
        mov rbx, 0
        mov cx, 0
        mov dx, 0

_outer_loop:
        mov dx, 0

_inner_loop:

        push cx
        push dx
        call dir_up
        add rsp, 4
        push rax
        push rbx
        call max
        add rsp, 16
        mov rbx, rax

        push cx
        push dx
        call dir_left
        add rsp, 4
        push rax
        push rbx
        call max
        add rsp, 16
        mov rbx, rax

        push cx
        push dx
        call dir_dia1
        add rsp, 4
        push rax
        push rbx
        call max
        add rsp, 16
        mov rbx, rax

        push cx
        push dx
        call dir_dia2
        add rsp, 4
        push rax
        push rbx
        call max
        add rsp, 16
        mov rbx, rax

        inc dx
        cmp dx, 20
        jb _inner_loop

        inc cx
        cmp cx, 20
        jb _outer_loop

        push rax
        call ltostr
        pop rax

        mov rcx, string
        call puts

        pop rbp
        ret
