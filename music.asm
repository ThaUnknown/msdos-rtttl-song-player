_code segment
assume  cs:_code, ds:_data, ss:_stack  
; http://merwin.bespin.org/t4a/specs/nokia_rtttl.txt

start:
    mov ax,_data
    mov ds,ax
    mov ax,_stack
    mov ss,ax 
    
    ; load params from command line 
    call cmlparam
    ; open file from parameters
    lea dx, filename   
    mov ax, 3D00h      ;open file
    int 21h
    mov filehandle, ax
    call findcontrolsection
    call setupmenu     
    call calcbpm                                  
    call playsong
    
exit:
    call cls                                
    mov ah, 4ch
    mov al, 0
    int 21h 

setupmenu proc near
    call cls
    mov ax, beat[0]
    call todec
    mov al, octave[0]
    shr al, 1
    add al, '1'
    mov defaultoctave[0], al
    lea dx, menustring
    mov ah, 9
    int 21h
    ret
setupmenu endp 

checkkey proc near
    mov ah, 1
    int 16h
    jz endcheckkey
    mov ah, 0
    int 16h
    
    cmp al, 'p'
    je pause
    cmp al, 'P'
    je pause
    cmp al, 'x'
    je exit
    cmp al, 'X'
    je exit   
    cmp al, '+'
    je hasten
    cmp al, '-'
    je retard  
    ret
retard:
    sub beat[0], 5 
    jmp tempochange
hasten:
    add beat[0], 5
tempochange:  
    call calcbpm 
    call setupmenu 
    ret

pause:
    mov ah, 0
    int 16h    
endcheckkey:
    ret
checkkey endp
    
cls proc near
    mov ax, 0003h
    int 10h
    mov ax, 1003h
    xor bx, bx
    int 10h
    ret
cls endp

tohex proc near
    ; converts ax from dec to hex ex: 0255h -> 00FFh
    push bx 
    mov bx, ax
    push cx
    push dx
    
    xor ax, ax
    mov cx, 4
hexmul:
    mov dx, 10d
    mul dx 
    rol bx, 4
    mov dx, bx
    and dx, 0Fh
    add ax, dx
    loop hexmul
    
    pop dx
    pop cx
    pop bx
    ret
tohex endp

todec proc near        
    ; converts ax from hex to dec ascii ex: 00FFh -> 3[0]3[2]3[5]3[5]h
    ; stores inside hexStore var
    ; dec is HR only, so might as well store it as ascii right away 
    push bx
    push cx
    push dx                                            
           
    mov hexStore[0], word ptr '$$'
    mov hexStore[1], word ptr '$$'
    mov bx, 10
    xor cx, cx        
divide:             
    xor dx, dx         
    div bx          
    add dl, '0'    
    push dx        
    inc cx            
    cmp ax, 0         
    jne divide     
    
    lea di, hexStore 
reverse:           
    pop [di]         
    inc di           
    loop reverse
    
    pop dx
    pop cx
    pop bx    
    ret  
todec endp

epilepsy proc near   
    push dx      
    mov dl, al
    push ax
    push bx
    push cx
    
    xor bx, bx          
    mov ax, screen
    mov es, ax

    mov cx, 80d
    shr dx, 1
    shl dx, 4
    mov di, 1
fillloop:
    mov byte ptr es:[di], dl
    add di, 2
    loop fillloop
    
    pop cx
    pop bx
    pop ax  
    pop dx
    ret    
epilepsy endp 

calcbpm proc near
    ; calculate the beats per minute, 
    ; so 60000000µs / beat, 0x3938700µs / beat  
    ; min BPM is 6 cuz otherwise overflow 
    push ax
    push bx
    push dx
    
    mov ax, 4000h  ; cut 2 0's, they are useless for us
    mov dx, 0004h
    mov bx, beat[0]
    shr bx, 2
    div bx
    mov notedur[0], ax
    
    pop dx
    pop bx
    pop ax
    ret
calcbpm endp

playsong proc near
    cmp char[0], 0
    je endplaysong
    call checkkey
    call parsegroup
    call epilepsy     
    call playgroup
    jmp playsong
endplaysong:
    ret
playsong endp

playgroup proc near
    cmp al, 30d
    je waitbios
    
    mov di, ref[bx]
    xor bx, bx
    mov bl, al
                       
    cli                     ; Don't disturb the setting sequence
    mov al, 0b6h
    out 43h, al
    mov ax, di[bx]
    
    out 42h, al
    mov al, ah
    out 42h, al
    sti
    
    in al, 61h              ; Speaker on
    or al, 03h
    out 61h, al
    
waitbios:
    call bioswait
    
    in al, 61h          ; 5) Read the current keyboard controller status.
    and al, 0fch        ; 5) Zero 0 and 1 bit, simply disabling the gate.
    out 61h, al
    
    ret  
        
playgroup endp

bioswait proc near
    push ax
    
    mov ax, cx
    call tohex
    mov cx, ax
    
    mov ax, word ptr notedur[0]
    xor dx, dx
    div cx
    mov cl, ah
    mov dh, al
    mov dl, 0h
    mov ah, 86h
    int 15h
    
    pop ax 
    ret    
bioswait endp 

parsegroup proc near
    ; cx duration
    ; bx octave pos
    ; ax note pos
    
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    mov bl, octave[0]
    mov cl, duration[0]
parsegroupduration:       
    cmp char[0], ' '
    je parsegroupdurationnext
    cmp char[0], ','
    je parsegroupdurationnext
    cmp char[0], '1'
    jb parsegroupnote
    cmp char[0], '8'
    ja parsegroupnote
    cmp dl, 1
    je skipfix
    mov dl, 1
    xor cl, cl
skipfix:
    shl cl, 4 
    add cl, char[0]
    sub cl, '0'
parsegroupdurationnext: 
    call readnext    
    jmp parsegroupduration

parsegroupnote:
    cmp char[0], ' '
    je parsegroupnotenext
    cmp char[0], '#'
    je parsenote
    cmp char[0], 'A'
    jb parsegroupoctave
    cmp char[0], 'P'
    jbe parsenote
    cmp char[0], 'a'
    jb parsegroupoctave
    cmp char[0], 'p'
    jbe parsenotesmall
parsegroupnotenext: 
    call readnext
    jmp parsegroupnote
    
parsegroupoctave: 
    cmp char[0], '.'
    je dottednote
    cmp char[0], ' '
    je parsegroupoctave
    cmp char[0], '1'
    jb parsegroupend
    cmp char[0], '8'
    ja parsegroupend
    mov bl, char[0]
    sub bx, '1'
    shl bx, 1
parsegroupend:
    call readnext
    ret
 
dottednote:
    mov dl, cl
    shr dl, 2
    sub cl, dl
    call readnext
    jmp parsegroupoctave
 
; this can be optimised to use indexes instead of HR ordering, TODO
parsenotesmall:
    sub char[0], 32d  ; to upper case
parsenote:
    cmp char[0], '#'
    je notesharp 
    mov al, char[0] 
    sub ax, 'A' ; to value 
    shl ax, 1
    jmp parsegroupnotenext   
notesharp:
    add ax, 14d
    jmp parsegroupnotenext
     
parsegroup endp

    
findcontrolsection proc near
    ; find 2 occurences of :, then navigate to the first one, load params, then to the second one
    ; leave file cursor on the last occurence of :
    mov readmode[0], 1
    xor ax, ax     
    xor dx, dx

findcolon:
    call readnext
    cmp char[0], 0
    je setposition
    inc ax
    cmp char[0], ':'
    jne findcolon
    
    cmp dx, 0
    je findcolonagain
    call setposition
    call soundparam
    mov dx, ax
setposition:
    push ax
    mov bx, filehandle
    xor cx, cx
    mov ax, 4200h
    int 21h
    pop ax        
    call readnext
    ret  
    
findcolonagain:
    mov dx, ax
    jmp findcolon  
    
findcontrolsection endp
    
cmlparam proc near
    ; cml param format [hardcoded: es:80h]: 1B:{len}, ?B:{input}, 1B:{\ret}
    ; out params ?B:{filename} ?B?:d={dur} ?B?:o={octave} ?B?:b={beat}      
    lea di, filename
    xor bx, bx            
nameparam:
    call readnext
    mov al, char[0]
    cmp al, 13d
    je endcmlparam
    cmp al, ' '
    jne writetoname
    call soundparam
endcmlparam: 
    ret
writetoname:   
    mov [bx+di-1], al 
    jmp nameparam            
cmlparam endp

soundparam proc near
    push ax
soundparamloop:
    xor ax, ax
    call checkend
    jc endsoundparam
    cmp char[0], ' '
    je soundparamnext
    cmp char[0], 'd'
    je durparam
    cmp char[0], 'D'
    je durparam
    cmp char[0], 'o'
    je octparam
    cmp char[0], 'O'
    je octparam
    cmp char[0], 'b'
    je beatparam
    cmp char[0], 'B'
    je beatparam
soundparamnext: 
    call readnext   
    jmp soundparamloop
endsoundparam:
    pop ax
    ret
durparam:
    call readparam
    mov duration[0], al
    call checkend
    jnc soundparamloop
    jmp endsoundparam 
    
octparam:
    call readparam
    dec al
    shl al, 1   
    mov octave[0], al
    call checkend
    jnc soundparamloop
    jmp endsoundparam  
      
beatparam:   
    call readparam
    call tohex 				
    mov beat[0], ax  
    call checkend
    jnc soundparamloop
    jmp endsoundparam
	      
; loads full param value into AX  
readparam:
    call readnext
    cmp char[0], '='
    je readparam
    cmp char[0], ' '
    je readparam   
    cmp char[0], ','
    je endreadparam   
    call checkend
    jc endreadparam
    rol ax, 4
    add al, char[0]
    sub al, '0'
    jmp readparam
endreadparam:
    ret
    
checkend:
   cmp char[0], 13d
   je foundend
   cmp char[0], ':'
   je foundend
   cmp char[0], 0
   je foundend
   clc
   ret
foundend:
   stc
   ret
soundparam endp

readnext proc near
    ; reads next char im w/e mode and stores it in char[0]
    cmp readmode[0], 0
    je cmlread
    
    ; file read 
    push ax
    push bx
    push cx
    push dx
    
    mov bx, filehandle
    lea dx, char 
    mov ah, 3Fh           
    mov cx, 1     
    int 21h
    cmp ax, 0
    je eof

returnreadnext:    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
eof:
    mov char[0], 0
    jmp returnreadnext    
cmlread:
    push ax
    mov al, es:[bx+82h]
    mov char[0], al
    inc bx
    pop ax
    ret     
        
readnext endp
_code ends

_data segment
    ;         A       B      C      D      E      F      G      A#       C#     D#        F#    G#
    oct1 dw 21694d,19327d,36485d,32505d,28958d,27333d,24351d,20477d,0d,34437d,30680d,0d,25799d,22984d
    oct2 dw 10847d, 9664d,18243d,16252d,14479d,13666d,12175d,10238d,0d,17219d,15340d,0d,12899d,11492d
    oct3 dw  5424d, 4832d, 9121d, 8126d, 7240d, 6833d, 6088d, 5119d,0d, 8609d, 7670d,0d, 6450d, 5746d
    oct4 dw  2712d, 2416d, 4561d, 4063d, 3620d, 3417d, 3044d, 2560d,0d, 4305d, 3835d,0d, 3225d, 2873d
    oct5 dw  1356d, 1208d, 2280d, 2032d, 1810d, 1708d, 1522d, 1280d,0d, 2152d, 1918d,0d, 1612d, 1437d
    oct6 dw   678d,  604d, 1140d, 1016d,  905d,  854d,  761d,  640d,0d, 1076d,  959d,0d,  806d,  718d
    oct7 dw   339d,  302d,  570d,  508d,  452d,  427d,  380d,  320d,0d,  538d,  479d,0d,  403d,  359d
    ref dw oct1, oct2, oct3, oct4, oct5, oct6, oct7
    readmode db 0             ; I/O read mode 0||1 [ 0 = CML, 1 = CurrFile ]
    char db 2 dup(0)          ; single char from I/O function
    duration db 4             ; hex default dur of a single note entry
    octave db 10              ; hex default oct of a single note entry
    beat dw 63d               ; hex default bpm of entire song
    notedur dw 0000h          ; 2 highest bytes of the duration of a single beat
    menustring db 80 dup('#'), 10d, 13d, "   [X] Exit   [P] Play/Pause   [+] Hasten   [-] Retard", 10d, 13d, 10d, 13d, "   Now Playing: "
    filename db 64 dup(0)
    menustring2 db 10d, 13d, "   Default Octave: "
    defaultoctave db '6'
    menustring3 db "   Current BPM: "
    hexStore dw "$$", "$$", "$$"
    screen equ 0B800h         ; magic number \o/
    filehandle dw ?
_data ends

_stack segment stack
    dw    100h dup(?)
_stack ends

end start         