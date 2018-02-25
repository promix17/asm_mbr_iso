%define BOOT_ADDR	0x7C00
%define	endl 10,13,0

jmp boot_start

copyright db "Promix17 boot iso v 0.2", endl
sendl db endl

boot_start:
	
	cld
	xor	cx, cx
	mov	ss, cx
	mov	es, cx
	mov	ds, cx
	mov	sp, BOOT_ADDR
	mov	bp, sp
	mov	si, BOOT_ADDR + copyright
	call	print
	
	jmp start
	
print:
	pusha
	print_char_loop:
		lodsb
		test al, al
		jz short pr_exit
		mov	ah, 0eh
		mov	bl, 7
		int	10h
	jmp	short print_char_loop
	pr_exit:
	popa
ret

print_char: ; char in al
	pusha
	mov	ah, 0eh
	mov	bl, 7
	int	10h
	popa
ret

start:

main_loop:	
	call cmos
	jmp	main_loop
	
ret

; library

string db 0,0,0,0,0,0,0,0
number dw 0

string_to_number:
	pusha
	mov si, BOOT_ADDR + string
	mov ax, 0
	xor cl, cl
	
s_to_n_loop:

		mov cl, byte [si]
		cmp cl, 0		
		je end_s_to_n
		sub cl, '0'
		mov bx,10
		mul bx
		add ax, cx
		inc si
	jmp s_to_n_loop
	
	mov [BOOT_ADDR + number], ax
	
end_s_to_n:	
	popa
ret

number_to_string:
	pusha
	xor     cx, cx
    mov     bx, 10
oi2:
		xor     dx,dx
		div     bx
		push    dx
		inc     cx
		test    ax, ax
    jnz     oi2

    mov di, BOOT_ADDR + string
oi3:
		pop     bx
		add     bl, '0'
		mov byte [di], bl
		inc di
    loop oi3
	xor bx, bx
	mov byte [di], bl
    popa
ret

cmos:

	mov al, 07h 
	call co
	mov al, '.'
	call print_char
	mov al, 08h
	call co
	mov al, '.'
	call print_char
	mov al, 09h 
	call co
	mov al, ' '
	call print_char
	mov al, 04h 
	call co
	mov al, ':'
	call print_char
	mov al, 02h 
	call co
	mov al, ':'
	call print_char
	mov al, 00h 
	call co
	mov	si, BOOT_ADDR + sendl
	call	print
	
	mov al, 07h 
	call input
	mov al, '.'
	call print_char
	mov al, 08h
	call input
	mov al, '.'
	call print_char
	mov al, 09h 
	call input
	mov al, ' '
	call print_char
	mov al, 04h 
	call input
	mov al, ':'
	call print_char
	mov al, 02h 
	call input
	mov al, ':'
	call print_char
	mov al, 00h 
	call input
	mov	si, BOOT_ADDR + sendl
	call	print
	
ret

co:
	out 70h, al
	xor ax, ax
	in al, 71h
    push       ax
    shr        al, 4
    add        al, '0'
    call print_char
    pop        ax
    and        al, 0Fh
    add        al, '0'
    call print_char
ret

input:
	push ax
	xor ax, ax
	mov ah, 10h
	int 16h
	mov bl, al
	sub bl, '0'
	shl bl, 4
	call print_char
	mov ah, 10h
	int 16h
	push ax
	call print_char
	pop ax
	sub al, '0'
	add al, bl
	mov bl, al
	
	mov al, 4bh 
	out 70h,al     
	mov al,01h 
	out 71h,al  
	pop ax
	out 070h, al 
	mov al, bl
	out 071h, al 
	mov al,0bh
	out 70h,al
	mov al,0 
ret
