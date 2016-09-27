   
   ; assembles directly to .SNA snapshot using the zeus assembler
   ; https://en.wikipedia.org/wiki/Zeus_Assembler

   ; Offset   Size   Description
   ;------------------------------------------------------------------------
   ;0        1      byte   I
   ;1        8      word   HL',DE',BC',AF'
   ;9        10     word   HL,DE,BC,IY,IX
   ;19       1      byte   Interrupt (bit 2 contains IFF2, 1=EI/0=DI)
   ;20       1      byte   R
   ;21       4      words  AF,SP
   ;25       1      byte   IntMode (0=IM0/1=IM1/2=IM2)
   ;26       1      byte   BorderColor (0..7, not used by Spectrum 1.7)
   ;27       49152  bytes  RAM dump 16384..65535
   ;------------------------------------------------------------------------

                    org $4000 - 27

SNA                 db 0
                    dw 0,0,0,0
                    dw 0,0,0,0,0
                    db 0
                    db 0
                    dw 0
                    dw Stack
                    db 1
                    db 0

                ;  loop $4000
                ;    db 0
                ;  lend
                    
                    org  $8000

NumRows             equ 20
NumCols             equ  16
ColOffset           equ 8
RowOffset           equ 2
TileMapWidth        equ 140
      
EntryPoint          jp Main

TileMapPtr          defw  $0000

TWOBYTES            macro()
                    pop bc
                    ld ( hl ), c
                    inc h
                    ld ( hl ), b
                    inc h
                    mend
        
CELL                macro()
                    ld a,h 
                    loop 4
                        TWOBYTES()
                    lend
                    ld h,a
                    mend
        
TWOCELLS            macro()
                    pop ix
                    pop iy
                    ld (StackSave3), sp
                    ld sp, ix
                    CELL()
                    inc l
                    ld sp, iy
                    CELL()
                    inc l
                    ld sp, (StackSave3)
                    mend

SCRADD              macro(X)
                        Value = $4000 + (((X)/8 ) << 11) + (  (X) mod 8 ) * 32 + ColOffset
                        defw Value
                    mend

FILL                macro()
                    Row = 0
                    repeat 
                        SCRADD( Row + RowOffset )
                        loop NumCols
                            defw $0000
                        lend
                        Row = Row + 1
                        until Row >= NumRows
                    mend

MOTION              macro()
                    t = 0
                    repeat
                        defw cos( t / 255.0 * 3.1412 * 2.0 ) * (NumRows-4)/2 * 8 + (NumRows-3)/2 * 8 + 8
                        t = t + 1
                    until t > 255
                    mend
                    align 256

DisplayList         FILL()

StackSave3          defw $0000

DrawTiles           ld (StackSave+1), sp
                    ld e, NumRows
                    ld hl, DisplayList
                    ld sp, hl
l2                  pop hl
                    loop NumCols/2
                        TWOCELLS()
                    lend
                    ;ld a, e
                    ;dec a
                    ;ld e,a
                    dec e
                    jp nz l2
StackSave           ld sp, $0000
                    ret  

SHTILE              macro()
                    loop 64 
                        db $00
                    lend
                    mend

mottable            MOTION()

                    align 8

                    db 0,0,0,0,0,0,0,0
flappy0             dg ______XX 
                    dg ____XXXX
                    dg ___XXXXX 
                    dg _XXXXXXX
                    dg X____XXX
                    dg X_____XX 
                    dg X_____XX 
                    dg _X___XXX 
                    dg __XXXXXX 
                    dg __XXXXXX 
                    dg ___XXXXX 
                    dg _____XXX 
                    db 0
                    db 0
                    db 0
                    db 0
                    db 0,0,0,0, 0,0,0,0

                    dg XXXX____
                    dg XX__X___
                    dg X____X__
                    dg X___X_X_
                    dg X___X_X_
                    dg XX____X_
                    dg XXXXXXX_
                    dg XX_____X
                    dg X_XXXXXX
                    dg XX_____X
                    dg XXXXXXX_
                    dg XX______
                    db 0
                    db 0
                    db 0
                    db 0 
                    db 0,0,0,0, 0,0,0,0

                    db 0,0,0,0,0,0,0,0
flappy1             dg ______XX 
                    dg ____XXXX
                    dg ___XXXXX 
                    dg __XXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX 
                    dg X_____XX
                    dg X_____XX
                    dg _XXXXXXX  
                    dg __XXXXXX 
                    dg ___XXXXX 
                    dg _____XXX 
                    db 0
                    db 0
                    db 0
                    db 0
                    db 0,0,0,0, 0,0,0,0

                    dg XXXX____
                    dg XX__X___
                    dg X____X__
                    dg X___X_X_
                    dg X___X_X_
                    dg XX____X_
                    dg XXXXXXX_
                    dg XX_____X
                    dg X_XXXXXX
                    dg XX_____X
                    dg XXXXXXX_
                    dg XX______
                    db 0
                    db 0
                    db 0
                    db 0 
                    db 0,0,0,0, 0,0,0,0

                    
                    db 0,0,0,0,0,0,0,0
flappy2             dg ______XX 
                    dg ____XXXX
                    dg ___XXXXX 
                    dg __XXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX 
                    dg _XXXXXXX
                    dg X_____XX
                    dg X____XXX  
                    dg X___XXXX 
                    dg _XXXXXXX 
                    dg _____XXX 
                    db 0
                    db 0
                    db 0
                    db 0
                    db 0,0,0,0, 0,0,0,0

                    dg XXXX____
                    dg XX__X___
                    dg X____X__
                    dg X___X_X_
                    dg X___X_X_
                    dg XX____X_
                    dg XXXXXXX_
                    dg XX_____X
                    dg X_XXXXXX
                    dg XX_____X
                    dg XXXXXXX_
                    dg XX______
                    db 0
                    db 0
                    db 0
                    db 0 
                    db 0,0,0,0, 0,0,0,0
                    
Src0                dg XX_X_X__
                    dg XXX_X___
                    dg XX_X_X__
                    dg XXX_X___
                    dg XX_X_X__
                    dg XXX_X___
                    dg XX_X_X__
                    dg XXX_X___
                    
Src1                dg _X_X_X_X
                    dg __X_X_X_
                    dg _X_X_X_X
                    dg __X_X_X_
                    dg _X_X_X_X
                    dg __X_X_X_
                    dg _X_X_X_X
                    dg __X_X_X_

Src2                dg _X_XXXXX
                    dg X_XXXXXX
                    dg _X_XXXXX
                    dg X_XXXXXX
                    dg _X_XXXXX
                    dg X_XXXXXX
                    dg _X_XXXXX
                    dg X_XXXXXX
                    
Cap0                dg XXXXXXXX
                    dg XX______
                    dg XXX_____
                    dg XX______
                    dg XXX_____
                    dg XX______
                    dg XXX_____
                    dg XXXXXXXX

Cap1                dg XXXXXXXX
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg XXXXXXXX

Cap2                dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                
Blank               dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________

                    align 256

T0                  SHTILE()
T1                  SHTILE()
T2                  SHTILE()
T3                  SHTILE()
T4                  SHTILE()
T5                  SHTILE()
T6                  SHTILE()
T7                  SHTILE()
T8                  SHTILE()
                
                    align 8

TILEMAP             macro()
                    loop NumRows
                        loop TileMapWidth
                            defw T3
                        lend
                    lend
                    mend

TileMap            TILEMAP()

MakeTile            ld b, 8
                    ld de,8
l5                  push bc
                    push hl
                    ld a, (ix+0)
                    ld (hl), a
                    add hl, de
                    ld a, 7
                    ld b, (ix+0)
                    ld c, (iy+0)
l4                  sla c
                    rl  b
                    ld (hl), b
                    add hl, de
                    dec a
                    jr nz l4
                    pop hl
                    pop bc
                    inc hl
                    inc iy
                    inc ix
                    djnz l5
                    ret 
                    
MAKETILE            macro(SrcTile0, SrcTile1, DstTile)
                    ld ix, SrcTile0
                    ld iy, SrcTile1
                    ld hl, DstTile
                    call MakeTile
                    mend

MakeCap             ld (hl), (T5 & $ff)
                    inc hl
                    ld (hl), ((T5>>8)&$ff)
                    inc hl
                    ld (hl), (T6 & $ff)
                    inc hl
                    ld (hl), ((T6>>8)&$ff)
                    inc hl
                    ld (hl), (T7 & $ff)
                    inc hl
                    ld (hl), ((T7>>8)&$ff)
                    inc hl
                    ld (hl), (T8 & $ff)
                    inc hl
                    ld (hl), ((T8>>8)&$ff)
                    ret

MakeStem            ld (hl), (T0 & $ff)
                    inc hl
                    ld (hl), ((T0>>8)&$ff)
                    inc hl
                    ld (hl), (T1 & $ff)
                    inc hl
                    ld (hl), ((T1>>8)&$ff)
                    inc hl
                    ld (hl), (T2 & $ff)
                    inc hl
                    ld (hl), ((T2>>8)&$ff)
                    inc hl
                    ld (hl), (T4 & $ff)
                    inc hl
                    ld (hl), ((T4>>8)&$ff)
                    ret

MakePipe            ld de, TileMapWidth*2
                    ld hl, TileMap
                    ld e, c
                    ld d, 0
                    add hl, de
                    add hl, de
                    ld b,a
                    push bc
                    dec b
l6                  push hl
                    call MakeStem
                    pop hl
                    ld de, TileMapWidth*2
                    add hl, de
                    djnz l6
                    push hl
                    call MakeCap
                    pop hl
                    ld de, TileMapWidth*2
                    add hl, de
                    ld b, 6
getrow              add hl, de
                    djnz getrow
                    push hl
                    call MakeCap
                    pop hl
                    ld de, TileMapWidth*2
                    add hl, de
                    pop bc
                    ld a, NumRows
                    sub b
                    sub 7
                    jp m  l9
                    jr z  l9
                    ld b,a
l8                  push hl
                    call MakeStem
                    pop hl
                    ld de, TileMapWidth*2
                    add hl, de
                    djnz l8
l9                  ret

Pipes               db 10, 12
                    db 5,  22
                    db 8,  32
                    db 2,  42
                    db 8,  52
                    db 10, 62
                    db 8, 72
                    db 2, 82
                    db 6, 92
                    db 2, 102
                    db 10, 112
                    db 5, 122
                    db $ff

MakeTiles           MAKETILE( Blank,  Src0, T0 )
                    MAKETILE( Src0,  Src1, T1 )
                    MAKETILE( Src1,  Src2, T2 )
                    MAKETILE( Src2,  Blank, T4 )
                    MAKETILE( Blank, Blank, T3 )
                    MAKETILE( Blank,  Cap0, T5 )
                    MAKETILE( Cap0,  Cap1, T6 )
                    MAKETILE( Cap1,  Cap2, T7 )
                    MAKETILE( Cap2,  Blank, T8 )
                    
                    ld ix, Pipes
pipeloop            ld a, (ix+0 )
                    cp $ff
                    ret z
                    ld c, (ix+1)
                    call MakePipe
                    inc ix
                    inc ix
                    jr pipeloop

Frames              defw flappy0
                    defw flappy1
                    defw flappy2

Offset              defw $0
Ypos                defw $0
Xpos                defw NumCols/2
SprPtr              defw flappy0
FrameIndex          db 0

NEXTCELL            macro()
                    ld a, c
                    add a, 8
                    ld c, a
                    ld a, b
                    adc a, 0
                    ld b, a
                    mend

CSETCELL            macro(X)
                    ld  a, (hl)
                    and $c0
                    xor ((X) & $c0)
                    jr nz skipcell+1
                    inc hl
                    ld a, (hl)
                    xor (((X)>>8)&$ff)
                    jr nz skipcell
                    dec hl
docell              ld (hl), c
                    inc hl
                    ld (hl), b
skipcell            dec hl
                    mend

SETCELL             macro()
                    ld (hl), c
                    inc hl
                    ld (hl), b
                    dec hl
                    mend


OneCol              CSETCELL(T3)
                    NEXTCELL()
                    add hl, de
                    CSETCELL(T3)
                    NEXTCELL()
                    add hl, de
                    CSETCELL(T3)
                    ret

DrawSprite          ld hl, (Ypos )
                    ld de, (Xpos )
                    sla e
                    rl  d
                    ld a, l
                    and 7
                    srl h
                    rr  l
                    srl h
                    rr  l
                    srl h
                    rr  l
                    ld b,l
                    inc b
                    dec b
                    ld hl, DisplayList+2
                    add hl, de
                    ld de, NumCols*2+2
                    jr z  skipmul
mulloop             add hl, de
                    djnz mulloop
skipmul             push hl
                    ld bc, (SprPtr)
                    neg
                    ccf
                    jr z nooffset
                    add a,c
                    ld c, a
                    ld a, b
                    adc a, $ff
                    ld b, a
nooffset            call OneCol
                    pop hl
                    inc hl
                    inc hl
                    NEXTCELL()
                    call OneCol
                    ret

TILE                macro()
                    pop bc
                    ld d,a
                    or c
                    ld (hl), a
                    inc hl
                    ld (hl), b
                    inc hl
                    ld a,d
                    mend

UpdateDisplayList   ld (StackSave2+1), sp
                    ld hl, (Offset)
                    ld a, l
                    and 7
                    sla a
                    sla a
                    sla a
                    srl h
                    rr l
                    srl h
                    rr l
                    srl h
                    rr l
                    add hl,hl
                    ld de, TileMap
                    add hl, de
                    ld sp, hl
                    exx
                    ld hl, DisplayList+2
                    ld e, NumRows
l0                  TILE()
                    loop NumCols-1
                        TILE()
                    lend
                    inc hl
                    inc hl
                    exx
                    ld de, TileMapWidth*2
                    add hl, de
                    ld sp, hl
                    exx 
                    dec e
                    jp nz l0
                    exx
StackSave2          ld sp, $0000
                    ret

                    defw $00, $00, $00, $00, $00, $00, $00, $00
                    defw $00, $00, $00, $00, $00, $00, $00, $00
                    defw $00, $00, $00, $00, $00, $00, $00, $00
                    defw $00, $00, $00, $00, $00, $00, $00, $00

Stack               defw EntryPoint

Animate             ld h, 0
                    add hl, hl
                    ld de, mottable
                    add hl, de
                    ld e, (hl )
                    inc hl
                    ld d, ( hl )
                    ld (Ypos), de

                    ld h, 0
                    ld a, (FrameIndex )
                    ld l,a
                    add hl,hl
                    ld de, Frames
                    add hl, de
                    ld e, (hl)
                    inc hl
                    ld d, ( hl )
                    ld (SprPtr ), de

                    ld a, (FrameIndex )
                    inc a
                    cp 3
                    jr nz  SetIndex
                    ld a, 0
SetIndex            ld ( FrameIndex ), a
                    ret

ClearScreen         ld a,0
                    out ($fe), a
                    ld de, $4001
                    ld hl, $4000 
                    ld bc, $1Aff
                    ld (hl), a
                    ldir
                    ret

Colours             db $28, $28 , $28 ,$28 , $28, $28, $28, $28
                    db $28, $28 , $28 , $28 , $28, $28, $28, $28
                    db $68, $68, $68, 32, 32, 32, 32, 32


SetAttributes       ld hl, $5800 + ColOffset + 32 * RowOffset
                    ld ix, Colours
                    ld b, NumRows
l7                  push bc
                    ld a, (ix+0)
                    ld ( hl ), a
                    ld bc, NumCols - 1
                    ld de, hl
                    inc de
                    ldir
                    ld de, 33 - NumCols
                    add hl, de
                    inc ix
                    pop bc
                    djnz l7
                    ret

Wait                ld b, $0
                    ld c, $b
l10                 djnz l10
                    dec c
                    jr nz l10
                    ret


Main                ld sp, Stack
                    call SetupIM2
                    ld hl, 0
                    call ClearScreen
                    call SetAttributes
                    call MakeTiles
MainLoop            ld b, 0
                    ld hl, 0
Next                ld (Offset), hl
                    push bc
                    push hl
                    ei 
                    halt
                    di
                    ;ld a,1
                    ;out ($fe), a
                    call Animate
                    call UpdateDisplayList
                    call DrawSprite
                    call Wait
                    ;ld a,2
                    ;out ($fe), a
                    ;ei
                    ;halt
                    ;di
					;ld a, 4
                    ;out ($fe), a 
                    call DrawTiles
                    ;ld a, 5
                    ;out ($fe), a 
                    pop hl
                    pop bc
                    inc hl
                    inc hl
                    ld a, $03
                    cp h
                    jr nc Next
                    ld hl, 0
                    jr Next

SetupIM2            di
                    im 2
                    ld a, $fd
                    ld i, a
                    ei
                    ret

                    org    $fdfd

                    loop 257
                        db $fe
                    lend

                    org    $fefe
                    ei
                    ret
                    
                    output_bin "flappy.sna",$4000-27,$c000 +27 ; The binary file       