	jmp HelloWorld
	c1:
	    resb 1
	c2:
	    resb 1
	c3:
	    resb 1
	book:
	    resb 5
	MAGIC1:
	    db 0x0000037B
	MAGIC2:
	    db 0x0000001E
	i:
	    resb 1
	j:
	    resb 1
	k:
	    resb 1
	iptr:
	    resb 2
	uptr:
	    resb 2
	count:
	    resb 2
hello:
	push bp
	mov bp, sp
	sub sp, 8
	mov ax, byte [bp + 10]
	mov byte [bp - 5], ax
	mov ax, 100
	mov word [bp - 7], ax
	mov ax, 1
	mov byte [bp - 8], ax
	mov ax, 1
	mov byte [i], ax
	mov ax, 2
	mov word [bp - 2], ax
	lea ax, [i]
	mov word [iptr], ax
	mov ax, word [bp - 2]
	push ax
	mov ax, byte [bp + 10]
	pop cx
	add ax, cx
	push ax
	mov ax, byte [bp + 9]
	pop cx
	add ax, cx
	push ax
	mov ax, word [bp + 7]
	push ax
	mov ax, 100
	pop cx
	mul ax, cx
	pop cx
	add ax, cx
	mov word [bp - 4], ax
.exit
	mov sp, bp
	pop bp
	ret
min:
	push bp
	mov bp, sp
	sub sp, 1
	mov ax, byte [bp + 5]
	push ax
	lea ax, [bp + 6]
	pop cx
	mov word [ax], cx
	jmp .exit
.exit
	lea ax, [bp + 6]
	mov sp, bp
	pop bp
	ret
HelloWorld:
	push bp
	mov bp, sp
	sub sp, 2
	mov ax, 0
	mov word [count], ax
	mov ax, 1
	push ax
	mov ax, 2
	pop cx
	add ax, cx
	push ax
	mov ax, 3
	pop cx
	add ax, cx
	mov byte [i], ax
	mov ax, 2
	push ax
	mov ax, 6
	pop cx
	mul ax, cx
	push ax
	mov ax, 1
	push ax
	mov ax, 5
	push ax
	mov ax, 7
	pop cx
	div cx, ax
	mov ax, cx
	pop cx
	sub ax, cx
	neg ax
	push ax
	mov ax, 11
	pop cx
	add ax, cx
	push ax
	mov ax, 4
	pop cx
	mul ax, cx
	pop cx
	add ax, cx
	mov byte [j], ax
	mov ax, word [count]
	push ax
	mov ax, 1
	pop cx
	add ax, cx
	mov word [count], ax
	mov ax, byte [c1]
	push ax
	mov ax, byte [c2]
	push ax
	mov ax, word [count]
	push ax
	mov ax, 1
	push ax
	mov ax, 1
	push ax
	call hello
	add sp, 11
	lea ax, [count]
	mov word [uptr], ax
	lea ax, [bp - 1]
	push ax
	mov ax, 18
	push ax
	mov ax, 19
	push ax
	call min
	add sp, 8
	mov byte [i], ax
	lea ax, [bp - 1]
	push ax
	mov ax, 1
	neg ax
	push ax
	mov ax, 2
	neg ax
	push ax
	call min
	add sp, 8
.exit:
	mov sp, bp
	pop bp
