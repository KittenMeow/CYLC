;;; NOTES
;
; The program file uses ! to , in the ASCII table, representing instructions 0 to 11.
; Arguments are capital letters A-P in the ASCII table.
;
;;; END NOTES

section .text
    global _start

;;;
;
; OPENING AND EXITING ROUTINES
;
;;;
_start:
    mov eax, 5 ; open file
    mov ebx, file
    mov ecx, 0 ; read-only
    int 0x80 ; interrupt

    mov [descriptor], eax

    mov eax, 3 ; read file
    mov ebx, [descriptor]
    mov ecx, ram ; read into ram
    mov edx, len ; length
    int 0x80 ; interrupt

    ; CX = Program counter
    ; DL = Instruction register
    ; BX = Argument 1
    ; AX = Argument 2
    xor ecx, ecx ; zero ecx
    mov byte [flags], 0x02 ; the zero flag is set
    call fetch

.exit:
    mov eax, 6 ; close file
    mov ebx, [descriptor]
    int 0x80 ; interrupt

    ; exit
    mov eax, 1
    mov ebx, 0
    int 0x80

error:
    ; for errors
    mov eax, 6 ; close file
    mov ebx, [descriptor]
    int 0x80 ; interrupt

    ; exit
    mov eax, 1
    mov ebx, 1 ; error code
    int 0x80
    
fetch:
    xor edx, edx ; zero other regs
    xor ebx, ebx
    xor eax, eax

    ;add cx, 1
    mov dl, [ram + ecx] ; get the first character in the instruction
    add cx, 1
    mov ax, [ram + ecx] ; get the next two characters
    add cx, 2
    mov bx, [ram + ecx] ; get the last two characters in the instruction
    add cx, 2 ; increment the program counter !!! problem with newlines

    sub dl, '!' ; offset
    ; offset bx, then remove any extra 0s
    sub bx, 0x4141 
    cmp bl, 0x0f
    jg .nextbl
    shl bl, 4
    shr bx, 4
.nextbl:
    ; offset ax, then remove any extra 0s
    sub ax, 0x4141
    cmp al, 0x0f
    jg .nextal
    shl al, 4
    shr ax, 4
.nextal:
    ; jump table to decode
    cmp edx, 0xf
    jg error
    jmp [edx * 4 + table]

;;;
;
; INSTRUCTIONS
;
;;;

branch:
    test [flags], ax ; if ax = 0x01 or ax = 0x02, and flags has the same bit set,
    jnz .jump ; jump

    cmp byte [flags], 0x03 ; if ax = 0x03, then both flags are set,
    jz .jump ; so jump

    cmp ax, 0 ; if ax was 0, then it was unconditional,
    jz .jump ; so jump.

    ; otherwise, just return.
    jmp fetch
.jump:
    push eax ; store eax to multiply by 5
    push ecx ; and ecx
    mov eax, ebx ; copy ebx into eax
    mov ecx, ebx ; copy ebx into ecx
    shl ebx, 3 ; * 8 (1 -> 8, 2 -> 16, 3 -> 24)
    shl eax, 1 ; * 2 (1 -> 2, 2 -> 4, 3 -> 6)
    sub ebx, eax ; (1 -> 6, 2 -> 12, 3 -> 18)
    sub ebx, ecx ; (1 -> 5, 2 -> 10, 3 -> 15)
    inc ecx ; (1 -> 6, 2 -> 11, 3 -> 16)
    pop ecx ; get old ecx back
    pop eax ; get old eax back

    push eax ; store eax
    mov eax, [stackp] ; put stack pointer into eax
    mov [ram + eax], cx ; store the return value on the stack
    add eax, 16 ; move to the next word in eax
    mov [stackp], eax ; put eax into the stack pointer
    pop eax ; restore eax

    mov cx, bx ; jump to the new IP
    jmp fetch

transfer:
    shl ebx, 1 ; align ebx
    shl eax, 1 ; align eax
    
    mov ax, word [regs + eax] ; mov the source register's value into ax
    mov word [regs + ebx], ax ; mov ax into the destination register

    jmp fetch

op_add:
    shl eax, 1
    mov ax, word [regs + eax] ; make ax into the register to add
    jmp op_adi ; add immediate in ax

op_adi:
    ; regs + 0 = A
    ; regs + 2 = X
    ; regs + 4 = Y
    ; regs + 6 = Z
    
    shl ebx, 1 ; align ebx

    add word [regs + ebx], ax 

    push ebx
    push ecx
    cmp word [regs + ebx], 65535
    jg .set_carry
    and byte [flags], 0b1110

.adi_cont_c:
    cmp word [regs + ebx], 0
    je .set_zero
    and byte [flags], 0b1101

.adi_cont_z:
    pop ecx
    pop ebx

    jmp fetch

.set_carry:
    or byte [flags], 0b0001
    jmp .adi_cont_c

.set_zero:
    or byte [flags], 0b0010
    jmp .adi_cont_z

op_sub:
    shl eax, 1
    mov ax, word [regs + eax] ; make ax into the register to subtract
    jmp op_sbi

op_sbi:
    xor ax, 0xffff ; flip all bits in ax
    inc ax ; two's complement
    jmp op_adi ; add two's complement of immediate in ax

op_eor:
    shl eax, 1
    mov ax, word [regs + eax] ; make ax into the register to xor
    jmp op_eori ; xor immediate in ax

op_eori:
    shl ebx, 1 ; align ebx
    xor word [regs + ebx], ax ; xor ax to the corresponding register
    jmp fetch ; continue

op_ltr:
    shl eax, 8 ; make ebx = bx, ax
    xor ebx, eax

    mov ax, word [regs] ; move value in accumulator to ax
    mov [ram + ebx], ax ; move ax to ram 
    jmp fetch ; return

op_lfr:
    shl eax, 8 ; make ebx = bx, ax
    xor ebx, eax

    mov ax, [ram + ebx] ; ax = item in ram
    mov word [regs], ax ; register = ax
    jmp fetch ; return

op_ldi:
    shl ebx, 1 ; align ebx

    mov word [regs + ebx], ax ; put ax into the register

    jmp fetch ; return

op_ret:
    sub word [stackp], 16 ; subtract 16 from the stack pointer
    push eax ; store eax
    mov eax, 0
    mov ax, [stackp] ; put the stack pointer into eax
    not ax ; 2's complement
    inc ax 
    mov cx, [ram + eax] ; reset CX to the old place
    pop eax ; restore eax

    jmp fetch ; return

op_push:
    add word [stackp], 16 ; add 16 to the stack pointer
    shl eax, 1 ; align eax
    push ebx ; store ebx
    mov ebx, 0 
    mov bx, [stackp] ; move the stack pointer into ebx
    mov ax, [regs + eax] ; put the value of the register into ax
    mov [ram + ebx], ax ; mov ax into the stack
    pop ebx ; restore ebx

    jmp fetch ; return

op_pop:
    shl eax, 1 ; align eax
    push ebx ; store ebx
    mov ebx, 0
    mov bx, [stackp] ; move the stack pointer into ebx
    mov bx, [ram + ebx] ; put the value of the stack into ax
    mov [regs + eax], bx ; mov ax into the stack
    pop ebx ; restore ebx

    sub word [stackp], 16 ; sub 16 to the stack pointer

    jmp fetch ; return

op_int:
    mov ebx, [intbase] ; make ebx into the intbase
    add ebx, eax ; add the interrupt number
    jmp .jump ; jump to that n

;;; 
;
; DATA
;
;;;

section .rodata
    table: ; jump table for "decoder"
        dd _start.exit ; !
        dd branch ; "
        dd transfer ; #
        dd op_add ; $
        dd op_adi ; %
        dd op_sub ; &
        dd op_sbi ; '
        dd op_eor ; (
        dd op_eori ; )
        dd op_ltr ; *
        dd op_lfr ; +
        dd op_ldi ; ,
        dd op_ret ; -
        dd op_push ; .
        dd op_pop ; /
        dd op_int ; 0

section .data
    regs dq 0 ; 16 bits for each register
    flags db 0 ; flags register (0ZC)
    sflags db 0 ; flags register for flags which are set by the programmer (interrupt disable, interrupt active)
    file db "./program.cylc", 0 ; input filename
    len equ 131072 ; length of RAM in bytes
    basep equ 130560 ; base pointer in bytes
    stackp dd 130560 ; stack pointer in bytes
    intbase equ 126720 ; where the interrupt handlers begin

section .bss
    ram resw 65536 ; random-access memory
    descriptor resb 4 ; store descriptor

; last 256 words of RAM is stack space