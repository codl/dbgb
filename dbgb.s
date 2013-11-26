.ROMDMG
.NAME "DESERTBUS"
.CARTRIDGETYPE 0
.RAMSIZE 0
.COMPUTEGBCHECKSUM
.COMPUTEGBCOMPLEMENTCHECK
.LICENSEECODENEW "00"
.EMPTYFILL $DB ; for the children!

.MEMORYMAP
SLOTSIZE $4000
DEFAULTSLOT 0
SLOT 0 $0000
SLOT 1 $4000
.ENDME

.ROMBANKSIZE $4000
.ROMBANKS 2

.BANK 0 SLOT 0

.ENUM $C000
Vel DB
XVel DB
AbsXVel DB
XPos DB
AbsXPos DB
DistanceTraveled DW
Points DB
.ENDE

.ORG $40 ; vblank interrupt
call vblank
reti

.org $48 ; LCD STAT interrupt
call lcdstat
reti

.ORG $100 ; start
nop
jp start

.ORG $0104 ; nintendo logo
.DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C
.DB $00,$0D,$00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6
.DB $DD,$DD,$D9,$99,$BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC
.DB $99,$9F,$BB,$B9,$33,$3E


.org $150
start:
    di ; disable interrupts
    ld sp,$FFF4 ; reset stack pointer to $FFF4

    waitvbl:
        ldh a,($44) ; A=($FF44)
        cp 144 ; compare A to 144
        jr c, waitvbl ; jump back to waitvbl if A<144

    ; we are vblanking, time to disable the screen
    xor a
    ldh ($40),a

    ld b, 8*2*10
    ld de,tiles
    ld hl,$8000
    ldspr:
    ld a,(de)
    ldi (hl),a
    inc de
    dec b
    jr nz,ldspr

    ld de,32*32
    ld hl,$9800
    ld b, $0
    -:
        inc b
        ld a, b
        xor 8
        jr nz,+
        xor a; a=0
        ld b, a
        +:
        ld a, b
        ldi (hl),a
        dec de
        ld a,e
        or d
        jr nz,-


    ld hl,$fe00
    ld b,40*4
    clspr:
    ld (hl),$0
    inc l
    dec b
    jr nz,clspr

    ld a,%00000011 ; interrupt enable. bit 0: vblank, bit 1: LCD STAT, bit 2: timer, bit 3: serial, bit 4: joypad
    ldh ($FF),a

    ld a,%00001000 ; LCD STAT interrupt settings: interrupt on h-blank
    ldh ($41),a

    ld a,%11100100 ; palette
    ldh ($47),a
    ldh ($48),a
    ldh ($49),a
    ;ld a,%10110011 ; turn on screen, window, background, set tiles to $8000
    ld a,%10010011 ; turn on screen, window, background, set tiles to $8000
    ldh ($40),a

    ld a,$80
    ld (XVel),a
    ld (XPos),a

    ld a,$0
    ld (Vel),a
    ld (AbsXVel),a
    ld (AbsXPos),a
    ld (DistanceTraveled),a
    ld (DistanceTraveled+1),a
    ld (Points),a

    ei ; enable interrupts again

menu:
    ;TODO
    ;jp menu

nameentry:
    ;TODO
    ;jp nameentry


main:
    halt
    jp main

vblank:
    ld a,%00100000 ; select directional buttons
    ldh ($0),a
    ldh a,($0) ; read button state
    ld b,a

    ld a, (XVel)
    ld c, a

    bit $1,b ; left
    jr nZ,+
        dec a
        jr nz,+
        ld a,$1
    +:

    bit $0,b ; right
    jr nZ,+
        inc a
        jr nz,+
        ld a,$ff
    +:

    cp c
    jr nz,+ ; no direction (or both directions) are pressed
        ld a, $80
    +:

    ld (XVel), a
    ld h, a

    cp $80
    jr z, endvbl
    jr c, absxspeedleft

    ;calculate absolute x speed to the right
        sub $80
        ld l, a
        jr +

    absxspeedleft: ; calculate abs x speed to the left
        ld a, $80
        sub h
        ld l, a

    +:

    srl l
    ld a, l
    ld (AbsXVel), a

    ld a, h
    cp $80
    jr c, addspeedleft

    ; add rightwards speed
        ld a, (XPos)
        add l
        jr nc, endgoing
        ld a, $ff
        jr endgoing

    addspeedleft:
        ld a, (XPos)
        sub l
        jr nc, endgoing
        ld a, $0




    endgoing:
        ld (XPos), a

        cp $80
        jr c, +
            sub $80
            jr ++
        +:
            ld b, a
            ld a, $80
            sub b
        ++:
            ld (AbsXPos), a



    endvbl:
        ld a,(XPos)
        srl a
        ldh ($43),a
        ret


lcdstat:
    ldh a, ($41)
    ld b, a
    ld a, %11
    and b
    jr nz, + ; we are at hsync
        ldh a, ($44)
        ; screen is $90 lines high
        cp $49
        jr c, +
        sub $49
        srl a
        srl a
        ld c, a ; c like count, getit
        cp 0
        jr z, +

        ld a, (AbsXPos)
        srl a
        srl a
        srl a
        srl a
        ld b, a
        ld a, (XPos)
        cp $80
        jr z, +
        jr c, ++
        -:
            add b
            dec c
            jr nz, -
            jr +++
        ++:
        -:
            sub b
            dec c
            jr nz, -
        +++:
        srl a
        ldh ($43),a

    +:
    ret



tiles:
; 00 test pattern
.DB %00001111
.DB %00000000 
.DB %00001111
.DB %00000000 
.DB %00001111
.DB %00000000 
.DB %00001111
.DB %00000000 
.DB %00001111
.DB %11111111 
.DB %00001111
.DB %11111111 
.DB %00001111
.DB %11111111 
.DB %00001111
.DB %11111111 
; 01 test pattern
.DB %01111110
.DB %11111111 
.DB %11111111
.DB %11111111 
.DB %11011011
.DB %11100111 
.DB %11100111
.DB %11000011 
.DB %11100111
.DB %11000011 
.DB %11011011
.DB %11100111 
.DB %11111111
.DB %11111111 
.DB %01111110
.DB %11111111 
; 02 white
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
.DB %00000000
.DB %00000000 
; 03 light grey
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
; 04 dark grey
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
.DB %00000000
.DB %11111111 
; 05 black
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
; 06 horizontal gradient
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
.DB %11110000
.DB %11001100
; 07 vertical gradient
.DB %11111111
.DB %11111111
.DB %11111111
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %11111111
.DB %11111111
.DB %00000000
.DB %11111111
.DB %00000000
.DB %00000000
.DB %00000000
.DB %00000000
.DB %00000000
; $08 vertical stripes
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
.DB %01010101
; $9 horizontal stripes
.DB %11111111
.DB %11111111
.DB %00000000
.DB %00000000
.DB %11111111
.DB %11111111
.DB %00000000
.DB %00000000
.DB %11111111
.DB %11111111
.DB %00000000
.DB %00000000
.DB %11111111
.DB %11111111
.DB %00000000
.DB %00000000
