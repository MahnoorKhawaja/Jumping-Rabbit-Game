[org 0x0100]
    jmp start
    res: dw  11352         ;total bytes in video memory
    v_res: dw 43           ;number of rows
    h_res: dw 132          ;number of columns
    one_third: dw 0        ;number of rows in one section
    vertical_buffer: times 14 dw 0      ;buffer to store columns of a section when shifting
    bg_buffer: times 1848 dw 0          ;buffer to store top 1/3rd section of screen
    middle_buffer: times 1716 dw 0      ;buffer to store middle 1/3rd section of the screen
    fg_buffer: times 1848 dw 0      ;buffer that stores the foreground
    plat_select: dw 1               ;brick selector 0 = green, 1 = yellow, 2 = orange, 3 = blue
    oldkbisr: dd 0              ;to store old keyboard interrupt routine
    rabbit_x: dw 63         ;x position of rabbit
    brick1_x: dw 56         ;x position of top brick
    brick2_x: dw 56         ;x position of middle brick
    brick3_x: dw 56         ;x position of bottom brick
    ;flags to check whether to move brick or not if flag 0 no movement, if flag 1 or -1 slow movement, if flag 2 or -2 fast movement 
    ;negative value means left movement positive value means right movement
    brick1_slide: dw 0      ;top brick movement flags   
    brick2_slide: dw 0      ;middle brick movement flags
    brick3_slide: dw 0      ;bottom brick movement flags
    ;left boundaries for bricks when moving left
    brick1_LBound: dw 50    ;top brick left boundary
    brick2_LBound: dw 56    ;middle brick left boundary
    brick3_LBound: dw 56    ;bottom brick left boundary
    ;right boundaries for bricks when moving right
    brick1_RBound: dw 62  ;top brick right boundary
    brick2_RBound: dw 56    ;middle brick right boundary
    brick3_RBound: dw 56    ;bottom brick right boundary
    gameover: db 0          ;flag to check whether game is over or not 
    score: dw 0             ;to store game score
    carrot_x: dw 63         ;x position of carrot
    carrot_flag: dw 0       ;flag to check whether to print carrot 
    grab_flag: dw 0         ;flag that checks whether carrot was grabbed
    oldtimer: dd 0          ;to store old timer interrupt routine
    animation_tick: dw 0
    slow_slide_tick: dw 0   ;count ticks for slow slide
    fast_slide_tick: dw 0   ;count ticks for fast slide
    slow_slide_speed: dw 50     ;set speed for slow slide, lower the faster
    fast_slide_speed: dw 35     ;set speed for fast slide, lower the faster
    blue_flag: dw 0
    timer_tick: dw 0
    rand_num: dw 0
;----------------------------------------------------------------------------------------------------------------------------------------------------
clrscr:		
    push es
    push ax
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 0
    mov ah, 0x07
    mov al, 0x20
    nextloc:	
        mov word [es:di], AX
        add di, 2					
        cmp di,	[res]            	
        jne nextloc

    pop di
    pop ax
    pop es
    ret
;---------------------------------------------------------------------------------------------------------------------------------------------------
delay:
    push cx
    mov cx, 0x0F00
    delay1:
        loop delay1
    pop cx
    ret
;---------------------------------------------------------------------------------------------------------------------------------------------------
scroll_delay:
    push cx
    mov cx, 0xFFF0
    fast_delay1:
        loop fast_delay1
    pop cx
    ret
;---------------------------------------------------------------------------------------------------------------------------------------------------
scr_divide:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xb800
    mov es, ax

    mov ax, [v_res]
    mov bx,0x3
    xor dx,dx  
    div bx
    mov [one_third], ax

    mov  ax, [h_res]
    mul word[one_third]
    shl ax, 1

    mov di,ax
    mov ah, 0x07
    mov al, '-'
    mov cx,[h_res]
    cld
    rep stosw

    mov ax, [one_third]
    add ax, [one_third]
    mul word[h_res]
    shl ax, 1
    mov di, ax

    mov ah, 0x07
    mov al, '-'
    mov cx,[h_res]
    cld
    rep stosw

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 
;--------------------------------------------------------------------------------------------------------------------------------
background:
    ;background colour
    push es
    push ax
    push bx
    push dx
    push di

    mov ax,ds
    mov es,ax

    mov ax,[v_res] ;43
    xor dx,dx
    mov bx,0x3 ;43/3=14
    div bx
    mov bx,ax
    mov ax,[h_res] ;132
    mul bx    
    shl ax,1   ;3696
    mov bx,ax
    add bx, bg_buffer

    mov di, bg_buffer
    mov ah,0x10      
    mov al,' '
    q1:
        mov [es:di],ax
        add di, 2
        cmp di, bx ;will work upto the first window
        jb q1
    mov di, bg_buffer
    mov ah,0x1F
    mov al,'*'
    q2:
        mov [es:di],ax
        add di, 100
        cmp di, bx ;will work upto the first window
        jb q2
    mov di, bg_buffer
    mov ah,0x1F
    mov al,'*'
    q3:  
        mov [es:di],ax
        add di, 150
        cmp di, bx ;will work upto the first window
        jb q3
    pop di
    pop dx
    pop bx
    pop ax
    pop es
    ret
;----------------------------------------------------------------------------------------------------------------------------------------------
buildings:
    push bp
    mov bp,sp
    sub sp,2 ;cus we have to amend ax later
    push es
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov ax,ds
    mov es,ax

    mov ax,[h_res]
    mul word [bp+8]
    add ax,[bp+6]
    shl ax,1

    mov di,ax ;di has the starting index
    add di, bg_buffer
    mov ax,[bp+10] ;length
    mov [bp-2],ax ;length
    mov bx,[bp+12] ;width of the building
    mov dx,[bp-2];length
    mov ah,[bp+4]
    mov al,0x20

    l1:
        mov cx,bx ;width
        mov si,di
        loop2:
            mov [es:si],ax
            add si,2
            dec cx
            jnz loop2
        add di,[h_res]
        add di,[h_res]
        dec dx
        jnz l1

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    add sp,2
    mov sp,bp
    pop bp
    ret 10
;------------------------------------------------------------------------------------------------------------------------------------------------------------
drawWindows:
    push bp
    mov bp,sp
    sub sp,6                    ;cus we have to amend ax later
    push es
    push ax
    push bx
    push cx
    push dx
    push si
    push di

    mov ax,[bp+8]   ;starting position of building
    mov [bp-2],ax   ;x position
    mov ax,[bp+6]
    mov [bp-4],ax   ;y position

    ;mov word [bp-6],2
    add word [bp-4],3    ;side space
    mov cx,2           ;changing this changes no of windows in each row

    outer:             ;increment row
        mov si,[bp-2]        ;moving x into si
        mov dx,3             ;changing this changes no of rows of windows
    
        inner:            ;increment column
            mov di,2
            w2:
                add si,1
                dec di
                 jnz w2
            push 0x1
            push 0x1
            push  si
            push word [bp-4]
            push 0x70;attribute
            call buildings
            dec dx
            jnz inner
        mov ax,3
        w:
            add word [bp-4],1   ;adding 1 to y
            dec ax
            jnz w
        dec cx
        jnz outer

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    add sp,6
    mov sp,bp
    pop bp
    ret 10
;--------------------------------------------------------------------------------------------------------------------------------------------------
all_buildings:
    push ax

    mov ax,10 ;width
    push ax
    mov ax ,13;lenght
    push ax
    ;starting position
    mov ax,1  ;push y pos
    push ax
    mov ax,2 ;push x pos
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings
    mov ax,10 ;width
    push ax
    mov ax ,13;lenght
    push ax
    ;starting position
    mov ax,1  ;push y pos
    push ax
    mov ax,2 ;push x pos
    push ax
    mov ax,0x70
    push ax   ;attribute
    call drawWindows


    mov ax,10 ;width
    push ax
    mov ax ,12;lenght
    push ax
    ;starting position
    mov ax,2;push x pos ;rows
    push ax
    mov ax,17;push y pos;cols
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings
    mov ax,10 ;width
    push ax
    mov ax ,12;lenght
    push ax
    ;starting position
    mov ax,2;push x pos ;rows
    push ax
    mov ax,17;push y pos;cols
    push ax
    mov ax,0x70
    push ax   ;attribute
    call drawWindows

    mov ax,10 ;width
    push ax
    mov ax ,12;lenght
    push ax
    ;starting position
    mov ax,2;push x pos ;rows
    push ax
    mov ax,32;push y pos;cols
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings
    mov ax,10 ;width
    push ax
    mov ax ,12;lenght
    push ax
    ;starting position
    mov ax,2;push x pos ;rows
    push ax
    mov ax,32;push y pos;cols
    push ax
    mov ax,0x70
    push ax   ;attribute
    call drawWindows


    mov ax,10 ;width
    push ax
    mov ax ,13;lenght
    push ax
    ;starting position
    mov ax,1;push x pos ;rows
    push ax
    mov ax,47;push y pos;cols
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings

    mov ax,10 ;width
    push ax
    mov ax ,13;lenght
    push ax
    ;starting position
    mov ax,1;push x pos ;rows
    push ax
    mov ax,47;push y pos;cols
    push ax
    mov ax,0x70
    push ax   ;attribute
    call drawWindows


    mov ax,10 ;width
    push ax
    mov ax ,7;lenght
    push ax
    ;starting position
    mov ax,7;push x pos ;rows
    push ax
    mov ax,62;push y pos;cols
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings

    mov ax,10 ;width
    push ax
    mov ax ,7;lenght
    push ax
    ;starting position
    mov ax,7;push x pos ;rows
    push ax
    mov ax,62;push y pos;cols
    push ax
    mov ax,0x70
    push ax   ;attribute
    call drawWindows


    mov ax,10 ;width
    push ax
    mov ax ,10;lenght
    push ax
    ;starting position
    mov ax,4;push x pos ;rows
    push ax
    mov ax,78;push y pos;cols
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings
    mov ax,10 ;width
    push ax
    mov ax ,10;lenght
    push ax
    ;starting position
    mov ax,4;push x pos ;rows
    push ax
    mov ax,78;push y pos;cols
    push ax
    mov ax,0x70
    push ax   ;attribute
    call drawWindows


    mov ax,10 ;width
    push ax
    mov ax ,11;lenght
    push ax
    ;starting position
    mov ax,3;push x pos ;rows
    push ax
    mov ax,94;push y pos;cols
    push ax
    mov ax,0x00
    push ax   ;attribute
    call buildings

    mov ax,10 ;width
    push ax
    mov ax ,11;length
    push ax
    ;starting position
    mov ax,3;push x pos ;rows
    push ax
    mov ax,94;push y pos;cols
    push ax
    mov ax,0x70
    push ax  ;attribute
    call drawWindows


    mov ax,10 ;width
    push ax
    mov ax ,9;lenght
    push ax
    ;starting position
    mov ax,5;push x pos ;rows
    push ax
    mov ax,117;push y pos;cols
    push ax
    mov ax,0x00
    push ax  ;attribute
    call buildings

    mov ax,10 ;width
    push ax
    mov ax ,9;length
    push ax
    ;starting position
    mov ax,5;push x pos ;rows
    push ax
    mov ax,117;push y pos;cols
    push ax
    mov ax,0x70
    push ax  ;attribute
    call drawWindows

    pop ax
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
print_background:
    push ax
    push cx
    push es
    push di
    push si

    mov ax, 0xb800
    mov es, ax

    mov si, bg_buffer
    mov di, 0

    mov cx, 1848
    rep movsw 

    pop si
    pop di
    pop es
    pop cx
    pop ax
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
road_bg:
    push es
    push ax
    push cx
    push di

    mov ax, ds
    mov es, ax
    mov di, middle_buffer

    mov ah, 0x08
    mov al, 219
    mov cx, 1716
    cld
    rep stosw

    call roadstripe

    pop di
    pop cx
    pop ax
    pop es
    ret
;--------------------------------------------------------------------------------------------------------------------------------
roadstripe:	
        push es
        push ax
        push cx
        push di

        mov ax, ds
        mov es, ax		

        mov ax, [one_third]
        shr ax, 1
        dec ax
        mul word[h_res]
        add ax, 5
        shl ax, 1

        mov di, ax
        add di, middle_buffer
        
        mov ah, 0x07
        mov al, 219
        mov cx, 5
    stripe:
        push cx
        mov cx, 20
        cld
        rep stosw
        pop cx
        add di, 12
        loop stripe
next:
        pop di
        pop cx
        pop ax
        pop es
        ret 
;--------------------------------------------------------------------------------------------------------------------------------------
car:
    push es
    push ax
    push cx
    push dx
    push di

    mov ax, ds
    mov es, ax

    mov ax, 2
    mul word[h_res]
    mov di, ax

    mov ax, [h_res]
    shr ax, 1
    sub ax, 10
    add ax, di
    shl ax, 1

    mov di, ax
    add di, middle_buffer
    mov ah, 0x09
    mov al, 219
    mov cx, 2 
    top_car:                ;top half of car
        push di
        push cx
        mov cx, 20
        cld 
        rep stosw
        pop cx
        pop di
        add di, [h_res]
        add di, [h_res]
        loop top_car

    sub di, [h_res]
    sub di, [h_res]
    add di, 2
    mov ah, 0x0B
    mov al, 219
    mov cx, 8
    cld
    rep stosw           ;window 1 of car
    
    add di, 4
    mov cx, 8
    cld 
    rep stosw           ;window 2 of car

    sub di, 46
    add di, [h_res]
    add di, [h_res]
    mov word[es:di-2], 0x04DB       ;backlight of car
    mov ah, 0x09
    mov al, 219
    mov cx, 3 
    bottom_car:                     ;bottom half of car
        push di
        push cx
        mov cx, 30
        cld 
        rep stosw
        pop cx
        cmp cx, 2
        jne no_light
        mov word[es:di], 0x0EDD     ;headlight of car
    no_light:
        pop di
        add di, [h_res]
        add di, [h_res]
        loop bottom_car

    sub di, [h_res]
    sub di, [h_res]
    add di, 6
    push di
    call tyre

    add di, 36
    push di
    call tyre

    pop di
    pop dx
    pop cx
    pop ax
    pop es
    ret
;--------------------------------------------------------------------------------------------------------------------------------------
tyre:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push di

    mov ax, ds
    mov es, ax

    mov di, [bp+4]
    mov ah, 0x00
    mov al, 219
    mov cx, 3
    outer1:
        push cx
        mov cx, 6
        cld 
        rep stosw
        sub di, 12
        add di, [h_res]
        add di, [h_res]
        pop cx
        loop outer1
    add di, 4
    sub di, [h_res]
    sub di, [h_res]
    sub di, [h_res]
    sub di, [h_res]
    mov word[es:di], 0x02DB
    mov word[es:di+2], 0x02DB

    pop di
    pop cx
    pop ax
    pop es
    pop bp
    ret 2
;---------------------------------------------------------------------------------------------------------------------------------------
print_middleground:
    push ax
    push cx
    push es
    push di
    push si

    mov ax, 0xb800
    mov es, ax

    mov si, middle_buffer
    
    mov ax, [one_third]
    inc ax
    mul word[h_res]
    shl ax, 1
    mov di, ax

    mov cx, 1716
    rep movsw 

    pop si
    pop di
    pop es
    pop cx
    pop ax
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
shift_right:
    push cx
    push di
    push si
    push es
    push ds

    push ds
    pop es

    mov si, middle_buffer
    add si, 262
    mov di, vertical_buffer

    mov cx, 13
    copy:       ;store rightmost column in buffer
        cld
        movsw 
        add si, 262
        loop copy

    mov si, 1714
    shl si, 1
    add si, middle_buffer
    mov di, si
    add di, 2
    mov cx, 13
    move_outer:         ;shift middle screen to right
        push cx
        mov cx, 131
        std
        rep movsw 
        pop cx
        sub si, 2 
        sub di, 2
        loop move_outer

    mov si, vertical_buffer
    mov di, middle_buffer
    mov cx, 13
    replace:            ;move the rightmost column to the leftmost column
        cld
        movsw
        add di, 262
        loop replace

    pop ds
    pop es
    pop si
    pop di
    pop cx
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
shift_left:
    push cx
    push di
    push si
    push es
    push ds

    mov ax, ds
    mov es, ax

    mov si, bg_buffer
    mov di, vertical_buffer
    mov cx, 14
    ;store left most column in buffer
    store:
        cld
        movsw 
        add si, 262
        loop store

    ;shifts screen to left
    mov cx,14 
    mov si, bg_buffer  
    add si, 2 
    mov di, bg_buffer     
    outerloop:
        push cx
        mov cx, 131
        rep movsw 
        pop cx
        add si, 2
        add di, 2
        loop outerloop

    mov cx,14
    mov si, vertical_buffer
    mov di, bg_buffer
    add di, 262
    ;shifts  leftmost corner to right most column
    restore:
        movsw 
        add di, 262
        loop restore

    pop ds
    pop es
    pop si
    pop di
    pop cx
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
print_score:
	push es
	push ax
	push bx
	push cx
	push dx
	push di

	mov ax, 0xb800
	mov es, ax
	mov ax, [score]
	mov bx, 10
	mov cx, 0
nextdigit:
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit
	
	mov ax, [v_res]
    sub ax, 13
    mov bx, [h_res]
    mul bx
    add ax, 125
    shl ax, 1
    mov di, ax
nextpos:
	pop dx
	mov dh, 0x17
	mov [es:di], dx
	add di, 2
	loop nextpos

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	ret 
;---------------------------------------------------------------------------------------------------------------------------------------------------
initial_foreground:             ;starting foreground stored in buffer
    push ax
    push bx
    push cx
    push dx
    push es
    push ds
    push di

    mov ax, ds
    mov es, ax          ;point es to ds
    mov di, fg_buffer

    mov ax, [one_third] ;14 lines
    mov bx, [h_res]     ;132
    mul bx              ;132x14
    mov cx, ax

    mov ah, 0x01
    mov al, 219
    rep stosw           ;blue background

    mov ax, [one_third]
    sub ax, 2
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov di, ax
    add di, fg_buffer

    mov ah, 0x0A        ;green ground
    mov al, 219
    mov cx, 264
    cld
    rep stosw

    mov ax, [one_third]
    sub ax, 8
    mov bx, [h_res]
    mul bx
    add ax, 56
    shl ax, 1
    mov di, ax

    mov ah, 0x02           ;green brick
    mov al, 219
    mov cx, 20
    brick1:
        mov [fg_buffer+di], ax
        add di, 2
        loop brick1

    mov ax, [one_third]
    sub ax, 14
    mov bx, [h_res]
    mul bx
    add ax, 56
    shl ax, 1
    mov di, ax

    mov ah, 0x02            ;green brick
    mov al, 219
    mov cx, 20
    brick2:
        mov [fg_buffer+di], ax
        add di, 2
        loop brick2

    pop di
    pop ds
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
print_foreground:
    push ax
    push bx
    push cx
    push dx
    push es
    push ds
    push di

    mov ax, [one_third]
    add ax, [one_third]
    inc ax              ;line 29
    mov bx, [h_res]     ;132
    mul bx              ;29x132
    shl ax, 1           ;29x132x2
    mov di, ax
    mov si, fg_buffer

    mov ax, 0xb800
    mov es, ax          ;video memory

    mov ax, [one_third] ;14 lines
    mov bx, [h_res]     ;132
    mul bx              ;132x14
    mov cx, ax
    rep movsw

    pop di
    pop ds
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
Platform:        ;store a brick in the first line of buffer 
;attribute sent as parameter
    push bp
    mov bp, sp
    push ax
    push cx
    push di

    mov ax, [brick1_x]      ;x position
    shl ax, 1
    mov di, ax

    mov ah, [bp+4]      ;attribute of brick
    mov al, 219
    mov cx, 20
    plat:
        mov [fg_buffer+di], ax
        add di, 2
        loop plat

    pop di
    pop cx
    pop ax
    pop bp
    ret 2
;--------------------------------------------------------------------------------------------------------------------------------
carrot:
    push ax
    push di

    cmp word[brick1_slide], 0
    je center
    cmp word[brick2_slide], 0
    je center
    mov ax, [rand_num]
    mov bx, [brick1_RBound]
    sub bx, [brick1_LBound]
    div bx
    add dx, [brick1_LBound]
    add dx, 7
    mov [carrot_x], dx
    jmp not_center
center:
    mov word[carrot_x], 63
not_center:
    mov ax, [carrot_x]
    shl ax, 1
    mov di, ax

    mov word[fg_buffer+di], 0x1ADC      
    add di, 2
    mov word[fg_buffer+di], 0x16DC      
    add di, 2
    mov word[fg_buffer+di], 0x16DC      
    add di, 2
    mov word[fg_buffer+di], 0x16DC      
    add di, 2
    mov word[fg_buffer+di], 0x16DC      
    add di, 2

    pop di
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
rabbit:             ;print normal rabbit 
    push ax
    push bx
    push cx
    push dx
    push es
    push di

    mov ax, [v_res]
    sub ax, 5
    mov bx, [h_res]
    mul bx              ;41x132
    add ax, [rabbit_x]          ;41x132 + 63
    shl ax, 1           ;(41x132 + 61)x2
    mov di, ax

    mov ax, 0xb800
    mov es, ax

    push di
    sub di, 264
    mov word[es:di], 0x1DDC
    add di, 10
    mov word[es:di], 0x1DDC
    pop di

    mov ah, 0x0F
    mov al, 219
    mov cx, 6
    cld
    rep stosw

    add di, 262
    mov cx, 6
    std
    rep stosw

    push di
    add di, 4
    mov word[es:di], 0x70DB
    add di, 6
    mov word[es:di], 0x70DB
    pop di

    add di, 266
    mov cx, 6
    cld
    rep stosw

    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
jumping_rabbit:            ;print rabbit jumping animation
    push ax
    push bx
    push cx
    push dx
    push es
    push di

    mov ax, [v_res]
    sub ax, 5
    mov bx, [h_res]
    mul bx              ;41x132
    add ax, [rabbit_x]          ;41x132 + 61
    shl ax, 1           ;(41x132 + 61)x2
    mov di, ax

    mov ax, 0xb800
    mov es, ax

    push di
    sub di, 264
    mov word[es:di], 0x1DDC
    add di, 10
    mov word[es:di], 0x1DDC
    pop di

    mov ah, 0x07
    mov al, 219
    mov cx, 6
    cld
    rep stosw

    add di, 262
    mov cx, 6
    std
    rep stosw

    push di
    add di, 4
    mov word[es:di], 0x70DC
    add di, 6
    mov word[es:di], 0x70DC
    pop di

    add di, 266
    mov cx, 6
    cld
    rep stosw

    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
dead_rabbit:            ;print rabbit jumping animation
    push ax
    push bx
    push cx
    push dx
    push es
    push di

    mov ax, [v_res]
    sub ax, 5
    mov bx, [h_res]
    mul bx              ;41x132
    add ax, [rabbit_x]          ;41x132 + 61
    shl ax, 1           ;(41x132 + 61)x2
    mov di, ax

    mov ax, 0xb800
    mov es, ax

    push di
    sub di, 264
    mov word[es:di], 0x1DDC
    add di, 10
    mov word[es:di], 0x1DDC
    pop di

    mov ah, 0x07
    mov al, 219
    mov cx, 6
    cld
    rep stosw

    add di, 262
    mov cx, 6
    std
    rep stosw

    push di
    add di, 4
    mov word[es:di], 0x7058
    add di, 6
    mov word[es:di], 0x7058
    pop di

    add di, 266
    mov cx, 6
    cld
    rep stosw

    sub di, 6
    mov word[es:di], 0x706F

    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
brick_selector:             ;selects and moves a brick to top row of buffer
    push ax
    push bx
    push dx

    mov ax, [rand_num]
    mov bx, 4
    div bx
    mov [plat_select], dx

    ;print a new brick at the top according to the platform selector
    cmp word[plat_select], 0
    jne next_check
    mov word[brick1_x], 56
    mov ax, 0x02            ;green brick
    push ax
    call Platform
    ; inc byte[plat_select]
    jmp finish
next_check:
    cmp word[plat_select], 1
    jne next_check2
    mov ax, 0x0E            ;yellow brick
    push ax
    call Platform
    ; inc byte[plat_select]
    jmp finish
next_check2:
    cmp word[plat_select], 2
    jne next_check3
    mov ax, 0x06            ;orange brick
    push ax
    call Platform
    ; inc byte[plat_select]
    jmp finish
next_check3:
    mov word[brick1_x], 56
    mov ax, 0x09            ;blue brick
    push ax
    call Platform
    ; mov byte[plat_select], 0
finish:
    pop dx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
move_values_down:       ;moves values related to bricks down
    push ax

    ;move x values of bricks down after jumping
    mov ax, [brick2_x]
    mov [brick3_x], ax
    mov ax, [brick1_x]
    mov [brick2_x], ax
    mov word[brick1_x], 56
    ;move slide flags down after jumping
    mov ax, [brick2_slide]
    mov [brick3_slide], ax
    mov ax, [brick1_slide]
    mov [brick2_slide], ax
    ;move left boundaries down after jumping
    mov ax, [brick2_LBound]
    mov [brick3_LBound], ax
    mov ax, [brick1_LBound]
    mov [brick2_LBound], ax
    ;move right boundaries down after jumping
    mov ax, [brick2_RBound]
    mov [brick3_RBound], ax
    mov ax, [brick1_RBound]
    mov [brick2_RBound], ax

    mov ax, [rand_num]
    mov bx, [brick1_RBound]
    sub bx, [brick1_LBound]
    div bx
    add dx, [brick1_LBound]
    mov [brick1_x], dx

    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
check_bricks:           ;check color of top brick and 
    push ax
    push bx
    push cx
    push dx
    push di

    mov di, [brick1_x]
    shl di, 1
    cmp word[fg_buffer+di], 0x0EDB      ;yellow
    jne next_slide
    mov word[brick1_slide], -1
    jmp skip_noslide
next_slide:
    cmp word[fg_buffer+di], 0x06DB      ;orange
    jne no_slide
    mov word[brick1_slide], 2
    jmp skip_noslide
no_slide:
    mov word[brick1_slide], 0
skip_noslide:
    mov ax, [one_third]
    sub ax, 2           ;second last row
    mov bx, [h_res]
    mul bx
    add ax, 56
    shl ax, 1
    mov di, ax

    cmp word[fg_buffer+di], 0x09DB
    jne not_blue
    mov cx, 20
    mov ax, 0xF9DB
    blink:
        mov [fg_buffer+di], ax
        add di, 2
        loop blink
    mov word[blue_flag], 1
    jmp skip_notblue
not_blue:
    mov word[blue_flag], 0
skip_notblue:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
check_carrot_grab:
    push ax
    push bx
    push cx
    push dx
    push es
    push di

    mov ax, [v_res]
    sub ax, 7
    mov bx, [h_res]
    mul bx
    add ax, [rabbit_x]
    shl ax, 1
    mov di, ax

    mov ax, 0xb800
    mov es, ax

    mov cx, 6
    check_carrot:
        cmp word[es:di], 0x01DB
        je next_carrot_check
        jmp grabbed
        next_carrot_check:
        add di, 2
        loop check_carrot
    jmp end_carrot_check
grabbed:
    mov word[grab_flag], 1
    inc word[score]
    cmp word[brick1_LBound], 15
    je change_fast_speed
    dec word[brick1_LBound]
    inc word[brick1_RBound]
change_fast_speed:
    cmp word[fast_slide_speed], 15
    je change_slow_speed
    dec word[fast_slide_speed]
change_slow_speed:
    cmp word[slow_slide_speed], 20
    je end_carrot_check
    dec word[slow_slide_speed]
end_carrot_check:
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
clear_carrot:
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, [one_third]
    sub ax, 6
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov di, ax

    mov cx, 132
    clean_carrot:
        mov word[fg_buffer+di], 0x01DB
        add di, 2
        loop clean_carrot

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
scroll_down:                ;scroll down the foreground
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es
    push ds

    mov ax, [rand_num]
    mov bx, 2
    div bx
    mov [carrot_flag], dx

    call move_values_down
    call check_carrot_grab
    mov cx, 6
    outer_scroll:
        mov ax, [one_third]
        dec ax
        mov bx, [h_res]
        mul bx
        push cx                 ;store value of cx for outer loop
        mov cx, ax

        mov si, ax
        shl si, 1
        sub si, 2
        mov di, si
        add di, [h_res]
        add di, [h_res]
        scroll:
            mov ax, [fg_buffer+si]
            mov [fg_buffer+di], ax
            sub si, 2
            sub di, 2
            loop scroll
        mov ah, 0x01
        mov al, 219
        mov si, 0
        mov cx, 132
        clear:
            mov [fg_buffer+si], ax
            add si, 2
            loop clear
        call scroll_delay
        call print_foreground
        call print_score
        call jumping_rabbit
        pop cx                  ;restore value of cx for outerloop
        cmp word[grab_flag], 1
        jne no_clean
        call clear_carrot
        mov word[grab_flag], 0
        no_clean:
        cmp cx, 2
        jne not_line5
        cmp word[carrot_flag], 1
        jne not_line5
        call carrot
        not_line5:
        loop outer_scroll
    mov word[timer_tick], 0
    call brick_selector
    call check_bricks
    call print_foreground
    call print_score
    call rabbit
    call check_rabbit
    cmp byte[gameover], 1
    jne no_add
    call dead_rabbit
no_add:
    pop ds
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
slow_slide_bricks:
    push ax
    push bx 
    push cx
    push dx
    push si
    push di

    cmp word[brick1_slide], 0
    je brick2SlideCheck
    cmp word[brick1_slide], -2
    je brick2SlideCheck
    cmp word[brick1_slide], 2
    je brick2SlideCheck
    cmp word[brick1_slide], 1
    je right1
    mov cx, 131
    mov si, 2
    mov di, 0
brick1_left:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    add si, 2
    add di, 2
    loop brick1_left
    dec word[brick1_x]
    mov ax, [brick1_LBound]
    cmp word[brick1_x], ax
    jne brick2SlideCheck
    mov word[brick1_slide], 1
    jmp brick2SlideCheck
right1:
    mov cx, 131
    mov si, 260
    mov di, 262
brick1_right:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    sub si, 2
    sub di, 2
    loop brick1_right
    inc word[brick1_x]
    mov ax, [brick1_RBound]
    cmp word[brick1_x], ax
    jne brick2SlideCheck
    mov word[brick1_slide], -1
brick2SlideCheck:
    cmp word[brick2_slide], 0
    je brick3SlideCheck
    cmp word[brick2_slide], -2
    je brick3SlideCheck
    cmp word[brick2_slide], 2
    je brick3SlideCheck
    cmp word[brick2_slide], 1
    je right2
    mov ax, [one_third]
    sub ax, 8
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    mov di, si
    add si, 2
    mov cx, 131
brick2_left:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    add si, 2
    add di, 2
    loop brick2_left
    dec word[brick2_x]
    mov ax, [brick2_LBound]
    cmp word[brick2_x], ax
    jne brick3SlideCheck
    mov word[brick2_slide], 1
    jmp brick3SlideCheck
right2: 
    mov ax, [one_third]
    sub ax, 8
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    add si, 260
    mov di, si
    add di, 2
    mov cx, 131
brick2_right:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    sub si, 2
    sub di, 2
    loop brick2_right
    inc word[brick2_x]
    mov ax, [brick2_RBound]
    cmp word[brick2_x], ax
    jne brick3SlideCheck
    mov word[brick2_slide], -1

brick3SlideCheck:
    cmp word[brick3_slide], 0
    je end_slide
    cmp word[brick3_slide], -2
    je end_slide
    cmp word[brick3_slide], 2
    je end_slide
    cmp word[brick3_slide], 1
    je right3
    mov ax, [one_third]
    sub ax, 2
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    mov di, si
    add si, 2
    mov cx, 131
brick3_left:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    add si, 2
    add di, 2
    loop brick3_left
    dec word[rabbit_x]
    dec word[brick3_x]
    mov ax, [brick3_LBound]
    cmp word[brick3_x], ax
    jne end_slide
    mov word[brick3_slide], 1
    jmp end_slide
right3: 
    mov ax, [one_third]
    sub ax, 2
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    add si, 260
    mov di, si
    add di, 2
    mov cx, 131
brick3_right:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    sub si, 2
    sub di, 2
    loop brick3_right
    inc word[rabbit_x]
    inc word[brick3_x]
    mov ax, [brick3_RBound]
    cmp word[brick3_x], ax
    jne end_slide
    mov word[brick3_slide], -1

end_slide:
    call print_foreground
    call print_score
    call rabbit
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
fast_slide_bricks:
    push ax
    push bx 
    push cx
    push dx
    push si
    push di

    cmp word[brick1_slide], 0
    je fastbrick2SlideCheck
    cmp word[brick1_slide], -1
    je fastbrick2SlideCheck
    cmp word[brick1_slide], 1
    je fastbrick2SlideCheck
    cmp word[brick1_slide], 2
    je fastright1
    mov cx, 131
    mov si, 2
    mov di, 0
fastbrick1_left:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    add si, 2
    add di, 2
    loop fastbrick1_left
    dec word[brick1_x]
    mov ax, [brick1_LBound]
    cmp word[brick1_x], ax
    jne fastbrick2SlideCheck
    mov word[brick1_slide], 2
    jmp fastbrick2SlideCheck
fastright1:
    mov cx, 131
    mov si, 260
    mov di, 262
fastbrick1_right:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    sub si, 2
    sub di, 2
    loop fastbrick1_right
    inc word[brick1_x]
    mov ax, [brick1_RBound]
    cmp word[brick1_x], ax
    jne fastbrick2SlideCheck
    mov word[brick1_slide], -2
fastbrick2SlideCheck:
    cmp word[brick2_slide], 0
    je fastbrick3SlideCheck
    cmp word[brick2_slide], -1
    je fastbrick3SlideCheck
    cmp word[brick2_slide], 1
    je fastbrick3SlideCheck
    cmp word[brick2_slide], 2
    je fastright2
    mov ax, [one_third]
    sub ax, 8
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    mov di, si
    add si, 2
    mov cx, 131
fastbrick2_left:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    add si, 2
    add di, 2
    loop fastbrick2_left
    dec word[brick2_x]
    mov ax, [brick2_LBound]
    cmp word[brick2_x], ax
    jne fastbrick3SlideCheck
    mov word[brick2_slide], 2
    jmp fastbrick3SlideCheck
fastright2: 
    mov ax, [one_third]
    sub ax, 8
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    add si, 260
    mov di, si
    add di, 2
    mov cx, 131
fastbrick2_right:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    sub si, 2
    sub di, 2
    loop fastbrick2_right
    inc word[brick2_x]
    mov ax, [brick2_RBound]
    cmp word[brick2_x], ax
    jne fastbrick3SlideCheck
    mov word[brick2_slide], -2

fastbrick3SlideCheck:
    cmp word[brick3_slide], 0
    je fastend_slide
    cmp word[brick3_slide], -1
    je fastend_slide
    cmp word[brick3_slide], 1
    je fastend_slide
    cmp word[brick3_slide], 2
    je fastright3
    mov ax, [one_third]
    sub ax, 2
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    mov di, si
    add si, 2
    mov cx, 131
fastbrick3_left:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    add si, 2
    add di, 2
    loop fastbrick3_left
    dec word[rabbit_x]
    dec word[brick3_x]
    mov ax, [brick3_LBound]
    cmp word[brick3_x], ax
    jne fastend_slide
    mov word[brick3_slide], 2
    jmp fastend_slide
fastright3: 
    mov ax, [one_third]
    sub ax, 2
    mov bx, [h_res]
    mul bx
    shl ax, 1
    mov si, ax
    add si, 260
    mov di, si
    add di, 2
    mov cx, 131
fastbrick3_right:
    mov ax, [fg_buffer+si]
    mov [fg_buffer+di], ax
    sub si, 2
    sub di, 2
    loop fastbrick3_right
    inc word[rabbit_x]
    inc word[brick3_x]
    mov ax, [brick3_RBound]
    cmp word[brick3_x], ax
    jne fastend_slide
    mov word[brick3_slide], -2

fastend_slide:
    call print_foreground
    call print_score
    call rabbit
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
check_rabbit:           ;check if rabbit is on a brick after jump
    push ax
    push bx
    push cx
    push dx
    push es
    push di

    mov ax, [v_res]
    sub ax, 3
    mov bx, [h_res]
    mul bx
    add ax, [brick3_x]
    shl ax, 1
    mov di, ax

    mov ax, 0xb800
    mov es, ax

    mov cx, 20
    check_jump:             ;check whether there is white pixel over botton brick
        cmp word[es:di], 0x0FDB
        je success
        add di, 2
        loop check_jump
    mov byte[gameover], 1   ;if no white pixel over bottom brick game over flag set
success:
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------------------------------------------------------------------------------------------------
kbisr:              ;new keyboard interrupt routine
    push ax
    push es
    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax

    in al, 0x60 	
    cmp al, 0x48            ;up key pressed
    jne next_key    
    mov al, 0x20
    out 0x20, al 
    call scroll_down 
    jmp exit
next_key:
    cmp al, 0x01                ;esc key pressed
    jne nomatch
    mov byte[gameover], 1       ;end game 
    jmp exit
nomatch:	
    pop es
    pop ax
    jmp far [cs:oldkbisr]       ;call the original ISR
exit:		
    mov al, 0x20
    out 0x20, al 
    pop es
    pop ax
    iret 
;--------------------------------------------------------------------------------------------------------------------------------
timer_interrupt:
    push ax
    push bx
    push cx
    push di
    push cs
    pop ds

    inc word[rand_num]
    inc word[animation_tick]

    cmp word[animation_tick], 2
    jb no_animation
    call Play_Animation
    mov word[animation_tick], 0
no_animation:
    cmp word[blue_flag], 0
    je no_timer
    inc word[timer_tick]
    cmp word[timer_tick], 75
    jb no_timer
    mov ax, [one_third]
    sub ax, 2           ;second last row
    mov bx, [h_res]
    mul bx
    add ax, 56
    shl ax, 1
    mov di, ax

    mov cx, 20
    mov ax, 0x01DB
    clean_blue:
        mov [fg_buffer+di], ax
        add di, 2
        loop clean_blue
    call print_foreground
    call print_score
    call dead_rabbit
    mov word[gameover], 1
no_timer:
    mov al, 0x20
    out 0x20, al
    pop di
    pop cx
    pop bx
    pop ax
    iret
;---------------------------------------------------------------------------------------------------------------------------------------
Print_scr:
    push ax
    push cx

    mov ah,0x00
    mov al, 0x54
    int 0x10

    call scr_divide
    call background
    call all_buildings
    call print_background
    call road_bg
    call car
    call print_middleground

    call initial_foreground
    call print_foreground
    call print_score
    call rabbit
    pop cx
    pop ax
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
Play_Animation:
    call shift_right
    call shift_left
    call print_background
    call print_middleground
    ret
;---------------------------------------------------------------------------------------------------------------------------------------
start:
    xor ax, ax
    mov es, ax
    mov ax, [es:8*4]
    mov [oldtimer], ax 
    mov ax, [es:8*4+2]
    mov [oldtimer+2], ax 
    mov ax, [es:9*4]
    mov [oldkbisr], ax 
    mov ax, [es:9*4+2]
    mov [oldkbisr+2], ax 

    cli 
    mov word[es:8*4], timer_interrupt
    mov [es:8*4+2], cs 
    mov word [es:9*4], kbisr 
    mov [es:9*4+2], cs 
    sti 

    call clrscr
    call Print_scr
    infinite:
        inc word[slow_slide_tick]
        inc word[fast_slide_tick]
       
        mov ax, [slow_slide_speed]
        cmp word[slow_slide_tick], ax
        jb no_slow_slide
        call slow_slide_bricks
        mov word[slow_slide_tick], 0
    no_slow_slide:
        mov ax, [fast_slide_speed]
        cmp word[fast_slide_tick], ax
        jb no_fast_slide
        call fast_slide_bricks
        mov word[fast_slide_tick], 0
    no_fast_slide:
        cmp byte[gameover], 1
        je terminate
        call delay
        jmp infinite

terminate:
    mov ax, [oldkbisr]
    mov bx, [oldkbisr+2]
    cli
    mov [es:9*4], ax
    mov [es:9*4+2], bx
    sti
    mov ax, [oldtimer]
    mov bx, [oldtimer+2]
    cli
    mov [es:8*4], ax
    mov [es:8*4+2], bx
    sti
    mov ax, 0x4c00
    int 0x21