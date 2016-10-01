   
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
                    
                    org  $8000

NumRows             equ 20
NumCols             equ 18
ColOffset           equ 7
RowOffset           equ 2
TileMapWidth        equ 128
      
EntryPoint          jp Main

TileMapPtr          defw  $0000

SCRADD              macro(X)
                        Value =$4000 + (((X)/8 ) << 11) + (  (X) mod 8 ) * 32 + ColOffset
                        defw Value
                    mend

ATTRADD             macro(X)
                         Value - $5800 + ((X)*32) + ColOffset
                         defw Value
                    mend

FILL                macro()
                    Row = 0
                    repeat 
                        align 256
                        defw SetScreen
                        SCRADD( Row + RowOffset )
                        loop NumCols
                            defw BlankCell
                            defw $0000
                        lend
                        defw NextRow
                        Row = Row + 1
                    until Row >= NumRows
                    align 256
                    defw StackSave
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



SHTILE              macro()
                    loop 64 
                        db $00
                    lend
                    mend

mottable            MOTION()

                    align 8

                    db 0,0,0,0,0,0,0,0
flappy0             db 0
                    db 0
                    dg ______XX 
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
                    db 0,0,0,0, 0,0,0,0

                    db 0
                    db 0
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
                    db 0,0,0,0, 0,0,0,0

                    db 0,0,0,0,0,0,0,0
flappy1             db 0
                    db 0
                    dg ______XX 
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
                    db 0,0,0,0, 0,0,0,0

                    db 0
                    db 0
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
                    db 0,0,0,0, 0,0,0,0

                    
                    db 0,0,0,0,0,0,0,0
flappy2             db 0
                    db 0
                    dg ______XX 
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
                    db 0,0,0,0, 0,0,0,0

                    db 0
                    db 0
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
                    db 0,0,0,0, 0,0,0,0
                    
Src0                dg _XXXXXXX
                    dg XXX_X___
                    dg XX_X_X__
                    dg XXX_X___
                    dg XX_X_X__
                    dg XXX_X___
                    dg XX_X_X__
                    dg XXX_X___
                    
Src1                dg XXXXX_X_
                    dg __X_X_X_
                    dg _X_X_X_X
                    dg __X_X_X_
                    dg _X_X_X_X
                    dg __X_X_X_
                    dg _X_X_X_X
                    dg __X_X_X_

Src2                dg ___XX___
                    dg __XXXX__
                    dg __XXXX__
                    dg __XXXX__
                    dg __XXXX__
                    dg __XXXX__
                    dg __XXXX__
                    dg __XXXX__
                    

Mask0               dg X_______
                    dg _XXXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX
                    dg _XXXXXXX

Mask1               dg ________
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX

Mask2               dg _____XXX    
                    dg XXXXX___     
                    dg XXXXX___     
                    dg XXXXX___     
                    dg XXXXX___     
                    dg XXXXX___     
                    dg XXXXX___     
                    dg XXXXX___                    

Cap0                dg XXXXXXXX
                    dg XXXXXX__
                    dg XXXXXXX_
                    dg XXXXXX__
                    dg XXXXXXX_
                    dg XXXXXX__
                    dg XXXXXXX_
                    dg XXXXXXXX

Cap1                dg XXXXXXXX
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg XXXXXXXX

Cap2                dg XXXXXX__
                    dg XXXXXX__
                    dg XXXXXX__
                    dg XXXXXX__
                    dg XXXXXX__
                    dg XXXXXX__
                    dg XXXXXX__
                    dg XXXXXX__
                
Blank               dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________
                    dg ________

BlankMask           dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX      
                    dg XXXXXXXX
                    dg XXXXXXXX
                    dg XXXXXXXX

                         
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
T9                  SHTILE()
T10                 SHTILE()
T11                 SHTILE()
T12                 SHTILE()

T13                 SHTILE()
T14                 SHTILE()
T15                 SHTILE()
T16                 SHTILE()
               


MT0                 defw VRepeat
                    defw T0
MT1                 defw VRepeat
                    defw T1
MT2                 defw VRepeat
                    defw T2
MT3                 defw BlankCell
                    defw T3
MT4                 defw VRepeat
                    defw T4
MT5                 defw OneCell
                    defw T5
MT6                 defw OneCell
                    defw T6
MT7                 defw OneCell
                    defw T7
MT8                 defw OneCell
                    defw T8
                    
MT9                 defw VRepeatMask
                    defw T13
MT10                defw VRepeatMask
                    defw T14
MT11                defw VRepeatMask
                    defw T15    
MT12                defw VRepeatMask
                    defw T16
                  
                    align 8

TILEMAP             macro()
                    loop NumRows
                        loop TileMapWidth
                            defw BlankCell
                            defw T3
                        lend
                    lend
                    mend

TileMap            TILEMAP()

StackSave3          defw $0000


TWOBYTES            macro()
                    pop bc
                    ld ( hl ), c
                    inc h
                    ld ( hl ), b
                    inc h
                    mend

CELL                macro()
                    ld a,h 
                    loop 3
                        TWOBYTES()
                    lend
                    pop bc
                    ld ( hl ), c
                    inc h
                    ld ( hl ), b
                    ld h,a
                    mend
        

OneCell             pop ix
                    ld (ss+1),sp
                    ld sp, ix
                    CELL()
                    inc l
ss                  ld sp, $0000
                    ret

BlankCell           pop bc
                    inc l
                    ret

FILLE               macro()
                    ld (hl),a
                    inc h
                    mend

VRepeat             pop bc
                    ld a, (bc)
                    ld d, h
                    loop 7
                         FILLE()
                    lend
                    ld (hl),a
                    ld h,d
                    inc l
                    ret 

FILLD               macro()
                    ld a, (hl)
                    and b
                    or e
                    ld (hl),a
                    inc h
                    mend


VRepeatMask         pop bc
                    ld a, (bc)
                    ld e, a
                    inc bc
                    ld a, (bc)
                    ld b, a
                    ld d, h
                    loop 7
                         FILLD()
                    lend
                    ld a, (hl)
                    and b
                    or e
                    ld (hl),a
                    ld h,d
                    inc l
                    ret 

StackSave4          defw $0000

NextRow             ld ( StackSave4 ), sp
                    ld de, (StackSave4)
                    ld e, 0
                    inc d
                    ld ix, de
                    ld sp, ix
                    ret

SetScreen           pop hl
                    ret


DrawTiles           ld (StackSave+1), sp
                    ld hl, DisplayList
                    ld sp, hl
                    ret
StackSave           ld sp, $0000
                    ret  

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

Interleave          ld b, 8
Interleave0         ld e, (ix+0)
                    ld d, (iy+0)
                    push bc

                    ld b, 4
Interleave1         ld (hl), e
                    inc hl
                    ld ( hl),d
                    inc hl
                    djnz Interleave1
                    
                    pop bc
                    ld de, 8
                    add ix, de
                    add iy, de
                    djnz Interleave0
                    ret 

MAKETILE            macro(SrcTile0, SrcTile1, DstTile)
                    ld ix, SrcTile0
                    ld iy, SrcTile1
                    ld hl, DstTile
                    call MakeTile
                    mend

MAKEMASK            macro( Src, Mask, Inter)
                    ld ix, Src
                    ld iy, Mask
                    ld hl, Inter
                    call Interleave
                    mend

WriteTile           ld a, (ix+0)
                    ld (hl),a
                    inc hl 
                    ld a, (ix+1)
                    ld (hl),a
                    inc hl 
                    ld a, (ix+2)
                    ld (hl),a
                    inc hl 
                    ld a, (ix+3)
                    ld (hl),a
                    inc hl
                    ret 

WRITETILE           macro( TilePtr )
                    ld ix, TilePtr
                    call WriteTile
                    mend


MakeCap             WRITETILE(MT5)
                    WRITETILE(MT6)
                    WRITETILE(MT7)
                    WRITETILE(MT8)
                    ret

MakeStem            WRITETILE(MT0)
                    WRITETILE(MT1)
                    WRITETILE(MT2)
                    WRITETILE(MT4)
                    ret


MakeStemMasked      WRITETILE(MT9)
                    WRITETILE(MT10)
                    WRITETILE(MT11)
                    WRITETILE(MT12)
                    ret


RowIndex            defb 0

MakePipe            ld hl, TileMap
                    ld e, c
                    ld d, 0
                    add hl, de
                    add hl, de
                    add hl, de
                    add hl, de
                    ld b,a
                    push bc
                    dec b

                    xor a
                    ld (RowIndex), a 

l6                  push hl
                    call MakeStem
                    ld a, (RowIndex)
                    inc a
                    ld ( RowIndex), a
                    pop hl
                    ld de, TileMapWidth*4
                    add hl, de
                    djnz l6
                    
                    push hl
                    call MakeCap
                    pop hl
                    
                    ld de, TileMapWidth*4
                    add hl, de
                    ld b, 6
getrow              add hl, de
                    djnz getrow
                    
                    ld a, (RowIndex )
                    add a, 8
                    ld ( RowIndex ), a
                    
                    push hl
                    call MakeCap
                    pop hl
                    

                    ld de, TileMapWidth*4
                    add hl, de

                    pop bc
                    
                    ld a, NumRows
                    sub b
                    sub 7
                    jp m  l9
                    jr z  l9
                    ld b,a

l8                  push hl
                    ld a, (RowIndex )
                    cp 16
                    jr nz MakePipe0
                    call MakeStemMasked
                    jr MakePipe1
MakePipe0           call MakeStem
MakePipe1           nop
                    ld a, (RowIndex )
                    inc a
                    ld (RowIndex),a
                    pop hl
                    ld de, TileMapWidth*4
                    add hl, de
                    djnz l8
l9                  ret

                    align 256

Pipes               db 2,  18
                    db 4,  30
                    db 7, 42
                    db 9, 54
                    db 9, 66
                    db 6, 74
                    db 4, 82
                    db 2, 90
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
                    MAKETILE( BlankMask, Mask0, T9 )
                    MAKETILE( Mask0, Mask1, T10)
                    MAKETILE( Mask1, Mask2, T11)
                    MAKETILE( Mask2, BlankMask, T12)
                    MAKEMASK( T0, T9, T13 )
                    MAKEMASK( T1, T10, T14 )
                    MAKEMASK( T2, T11, T15 )
                    MAKEMASK( T4, T12, T16 )

                    ld ix, Pipes
pipeloop            ld a, (ix+0 )
                    cp $ff
                    ret z
                    ld c, (ix+1)
                    push ix
                    call MakePipe
                    pop ix
                    inc ix
                    inc ix
                    jr pipeloop

Frames              defw flappy0
                    defw flappy1
                    defw flappy2

Offset              defw $0
Ypos                defw $4000
Yvel                defw $0
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
                    xor ((X&$ff))
                    jr nz skipcell+1
                    inc hl
                    ld a, (hl)
                    xor (((X)>>8)&$ff)
                    jr nz skipcell
                    dec hl
docell              ld (hl), ((OneCell) & $ff) 
                    inc hl
                    ld (hl), (((OneCell)>>8) & $ff)
                    inc hl
                    ld (hl), c
                    inc hl
                    ld (hl), b
                    dec hl
                    dec hl
skipcell            dec hl
                    mend

SETCELL             macro()
                    ld (hl), ((OneCell) & $ff) 
                    inc hl
                    ld (hl), (((OneCell)>>8) & $ff)
                    inc hl
                    ld (hl), c
                    inc hl
                    ld (hl), b
                    dec hl
                    dec hl
                    dec hl
                    mend


OneCol              CSETCELL(BlankCell)
                    NEXTCELL()
                    add hl, de
                    CSETCELL(BlankCell)
                    NEXTCELL()
                    add hl, de
                    CSETCELL(BlankCell)
                    ret

DrawSprite          ld hl, (Ypos )
                    ld de, (Xpos )
                    sla e
                    rl  d
                    sla e
                    rl  d
                    ld a, h
                    and 7
                    srl h
                 ;   rr  l
                    srl h
                  ;  rr  l
                    srl h
                   ; rr  l
                    ld b,h
                    inc b
                    dec b
                    ld hl, DisplayList+4
                    add hl, de
                    ld de, 256 ;NumCols*4+4
                    jr z  skipmul
mulloop             inc h
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
                    inc l
                    inc l
                    inc l
                    inc l
                    NEXTCELL()
                    call OneCol
                    ret

TILE                macro()
                    ld d,a
                    pop bc
                    ld ( hl ), c
                    inc l
                    ld ( hl ), b
                    inc l
                    pop bc
                    or c
                    ld (hl), a
                    inc l
                    ld (hl), b
                    inc l
                    ld a, d
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
                    add hl,hl
                    ld de, TileMap
                    add hl, de
                    ld sp, hl
                    exx
                    ld hl, DisplayList+4
                    ld e, NumRows
l0                  loop NumCols
                        TILE()
                    lend
                    ld l, 4
                    inc h
                    exx
                    ld de, TileMapWidth*4
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

PressedFlap         defb $0
PreviousKey         defb $0

ReadKeys            ld a, $7f
                    in a, ($fe)
                    cpl 
                    and 1
               
                    ld b,a 
                    ld hl, PreviousKey
                    ld a, ( PreviousKey)
                    cpl 
                    and b
                    ld a, b
                    ld ( PreviousKey ), a
                    jr z ReadKeys0
                    ld a, $ff
                    ld ( PressedFlap ), a
                    ret 
ReadKeys0           xor a
                    ld (PressedFlap ),a
                    ret 

NegDe               ld a, d
                    cpl 
                    ld d, a
                    ld a, e
                    cpl
                    ld e,a
                    inc de
                    ret

NegHL               ld a, h
                    cpl
                    ld h,a
                    ld a,l
                    cpl
                    ld l,a
                    inc hl
                    ret

AbsHL               ld a,h
                    and $80
                    ret z
                    call NegHL
                    ret 

Gravity             equ  16
TerminalVelocity    equ  2
Thrust              equ  512
MaxQ                equ  4

Physics             ld de, (Yvel)
                    ld hl, (Ypos)
                    add hl, de

                    ld a,h 
                    cp 150
                    jr nc Physics0
                    cp 5
                    jr c Physics0


                    ld (Ypos), hl

Physics0            ld hl, (Yvel)
                    ld de, Gravity
                    add hl, de

                    ld a, (PressedFlap)
                    and a
                    jr z Physics2
                    ld de, Thrust
                    call NegDe
                    add hl, de 
Physics2            push hl
                    ld a, h
                    and $80
                    jr z Physics3
                    call AbsHL 
                    ld a, h
                    cp MaxQ
                    jr nc Physics1
                    jr Physics4
Physics3            ld a,h
                    cp TerminalVelocity
                    jr c Physics4
Physics1            pop hl
                    ret 
Physics4            pop hl 
                    ld (Yvel ), hl
                    ret 

Collision           ld hl, ( Xpos )
                    sla l
                    rl h
                    sla l
                    rr h
                    sla l
                    rr h
                    ld de, ( Offset )
                    add hl, de
                    ld de, 0
                    add hl, de
                    srl  h
                    rr l
                    srl h
                    rr l
                    srl h
                    rr l
                    add hl,hl
                    add hl,hl
                    push hl
                    ld hl, ( Ypos )
                    ld de, $0000
                    add hl, de
                    srl h
                    srl h
                    srl h
                    ld b, h
                    ld hl, TileMap
                    ld de, TileMapWidth*4
Collision0          add hl, de
                    djnz Collision0
                    pop de
                    add hl, de
                    ld a, (hl)
                    cp (BlankCell&$ff)
                    jr nz Collision1
                    inc hl
                    ld a,(hl)
                    cp ((BlankCell>>8)&$ff)
                    jr nz Collision2
                    scf
                    ccf
                    ret
Collision2          nop ;dec hl
Collision1          nop ;ld a, (BlankCell&$ff)
                    ;ld ( hl ),a
                    ;inc hl
                    ;ld a, ((BlankCell>>8)&$ff)
                    ;ld ( hl),a 
                    ld a, 7
                    out ($fe), a
                    scf
                    ret 
               



Animate             ld h, 0
                    ld a, (FrameIndex )
                    ld l,a
                    add hl,hl
                    ld de, Frames
                    add hl, de
                    ld e, (hl)
                    inc hl
                    ld d, ( hl )
                    ld (SprPtr ), de

                    ld a, (Yvel+1)
                    and $80
                    ret z
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

Colours             db $68, $68 , $68 ,$68 , $68, $68, $68, $68
                    db $68, $68 , $68 ,$68 , $68, $68, $68, $68
                    db $78, $78,  $78, $78,  32,  32,  32,  32


SetAttributes       ld hl, $5800 + ColOffset + 32 * RowOffset
                    ld ix, Colours
                    ld b, NumRows + 1
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


align 256

Vista               dg XXX_____ ________ ________ ________ ______XX XXXXXXXX
                    dg ___XX___ _____XXX XXXXXXXX XXX_____ ___XXX__ ________
                    dg ____XX__ ___X____ ________ ___XX___ _X______ ________
                    dg ______XX _X______ ________ _____XX_ X_______ ________
                    dg ________ X_______ ________ _______X ________ ________
                    dg _______X ________ ________ _______X ________ ________
                    dg ______X_ ________ ________ ______X_ ________ ________
                    dg _____X__ ________ ________ _____X__ ________ ________


Ground              dg XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX
                    dg ________ ________ ________ ________ ________ ________
                    dg __XXXX__ __XXXX__ __XXXX__ __XXXX__ __XXXX__ __XXXX__
                    dg _XXXX___ _XXXX___ _XXXX___ _XXXX___ _XXXX___ _XXXX___
                    dg XXXX____ XXXX____ XXXX____ XXXX____ XXXX____ XXXX____
                    dg XXX____X XXX____X XXX____X XXX____X XXX____X XXX____X
                    dg ________ ________ ________ ________ ________ ________
                    dg XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX

                    db $0

ScrollGround        ld b,8 
                    ld hl, Ground + 6*8 - 1
                    jp ScrollVista1

ScrollVista         ld b,8
                    ld hl, Vista + 6*8 - 1
 ScrollVista1       ld de, hl
                    scf
                    ccf
                    rl (hl)
                    dec l
                    rl (hl)
                    dec l
                    rl (hl)
                    dec l
                    rl (hl)
                    dec l
                    rl (hl)
                    dec l
                    rl (hl)
                    dec l
                    ld a, 0
                    rla 
                    ex de,hl
                     or (hl)
                    ld (hl),a
                    ex de,hl
                    djnz ScrollVista1
                    ret                     

DrawGround          ld ix, $50C0 + ColOffset + NumCols
                    ld iy, Ground 
                    jp PushScroller

DrawVista           ld ix, $5040  + ColOffset + NumCols
                    ld iy, Vista ; //+ 6*8-1
PushScroller        ld (ss0+1), sp
                    loop 8
                    ld sp, iy
                    pop hl
                    pop de
                    pop bc
                         ld sp, ix
                         loop NumCols/6 
                              push bc
                              push de
                              push hl
                         lend
                         inc ixh
                    ld de, 6
                    add iy,de
                    lend
ss0                 ld sp, 0 
                    ret




Reset               call ClearScreen
                    call SetAttributes
                    ld hl,0
                    ld ( Offset ), hl
                    ld a, $00
                    ld (Ypos), a
                    ld a, $40
                    ld (Ypos+1),a
                    xor a
                    ld (Yvel), a
                    ld (Yvel+1),a
                    ret 

Main                ld sp, Stack
                    call SetupIM2
                    di
                    call MakeTiles
NewGame             call Reset 
MainLoop            ld b, 0
                    ld hl, 0
Next                ld (Offset), hl
                    push bc
                    push hl
                    ;ei 
                    ;halt
                    ;di
                ;    ld a,1
                 ;   out ($fe), a
                    call ReadKeys
                    call Physics
                    call Animate
                    call UpdateDisplayList
                    call DrawSprite
                    call Collision
                    jr c Dead
                   ; ld a, 6
                   ; out ($fe), a
                    pop hl
                    push hl
                    ld a, l
                    and 1 
                    call nz ScrollVista
                    call ScrollGround
                   ; call Wait
                  ;  ld a,2
                  ;  out ($fe), a
                    ei
                    halt
                    di
			;	ld a, 3
                ;    out ($fe), a
                    call DrawVista 
                 ;   ld a, 4
                 ;   out ($fe), a
                    call DrawTiles
                  ;  ld a, 5
                  ;  out ($fe), a
                    call DrawGround
                   ; ld a, 4
                   ; out ($fe), a 
                    pop hl
                    pop bc
                    inc hl
                    ld a, $02
                    cp h
                    jr nc Next
                    ld hl, 0
                    jr Next
Dead                pop hl
                    pop bc
                    ld hl, $5800
                    ld de, $5801
                    ld a, 2
                    ld ( hl ),a
                    ld bc , $2ff
                    ldir 
                    ei 
                    halt
                    di
                    jr NewGame 

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