.segment "HEADER"
  .byte $4E, $45, $53, $1A  ; iNES header identifier
  .byte 2                   ; 2x 16KB PRG code
  .byte 1                   ; 1x 8KB CHR data
  .byte $01, $00            ; Mapper 0, vertical mirroring

.segment "VECTORS"
  .addr nmi
  .addr reset
  .addr 0

.segment "ZEROPAGE"
pointerLo: .res 1
pointerHi: .res 1 


.segment "STARTUP"

.segment "CODE"

reset:
  sei
  cld
  ldx #$40
  stx $4017
  ldx #$ff
  txs
  inx
  stx $2000
  stx $2001
  stx $4010

vblankwait1:
  bit $2002
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

vblankwait2:
  bit $2002
  bpl vblankwait2

main:
load_palettes:
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop

LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$D0              ; Compare X to hex 
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte ($20) of $2000 address
  LDA #$00
  STA $2006             ; write the low byte ($00) of $2000 address
  LDX #$00              ; start out at 0
LoadBackgroundLoop:
  LDA background, x       ; load data from address (background + the value in x)
  STA $2007               ; write to PPU
  INX                     ; X++
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                          ; if compare was equal to 00, keep going down
  LDX #$00                ; start out at 0
LoadBackgroundLoop2:      ; Do this one more time for the next 256 bytes
  LDA background+256, x   
  STA $2007               
  INX
  BNE LoadBackgroundLoop2 
                          
  LDX #$00                
LoadBackgroundLoop3:      ; Do this one more time for the next 256 bytes
  LDA background+512, x   
  STA $2007               
  INX
  BNE LoadBackgroundLoop3
                          
  LDX #$00            
LoadBackgroundLoop4:      ; Do this one more time for the next 256 bytes
  LDA background+768, x   
  STA $2007               
  INX
  BNE LoadBackgroundLoop4


LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attributes, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down



enable_rendering:
  lda #%10000000
  sta $2000
  lda #%00010000
  sta $2001

forever:
  jmp forever

nmi:
  LDA #$00
  STA $2003  ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014  ; set the high byte (02) of the RAM address, start the transfer
	LDA #$00
	STA $2005
	STA $2005
  
  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

  RTI             ; return from interrupt


palettes:
  ; Background Palette
  .byte $0f, $00, $20, $10 ;P0
  .byte $0f, $01, $21, $30 ;P1
  .byte $0f, $07, $36, $18 ;P2
  .byte $0f, $15, $15, $15

  ; Load your Sprite Palette data from the .chr file here
  .byte $0f, $00, $20, $10 ;P0
  .byte $0f, $01, $21, $30 ;P1
  .byte $0f, $07, $36, $18 ;P2
  .byte $0f, $15, $15, $15 ;P3

sprites:
       ; Y  tile  atrr  X
  .byte $50, $01, $42, $1c   ;character facing left
  .byte $50, $02, $42, $14   ; atrr is %01000010 to flip horizontal and use palette 2
  .byte $58, $11, $42, $1c  
  .byte $58, $12, $42, $14   

  .byte $50, $03, $42, $2c   
  .byte $50, $04, $42, $24   
  .byte $58, $13, $42, $2c   
  .byte $58, $14, $42, $24   

  .byte $50, $05, $42, $3c   
  .byte $50, $06, $42, $34   
  .byte $58, $15, $42, $3c   
  .byte $58, $16, $42, $34   

  .byte $50, $07, $42, $4c   
  .byte $50, $08, $42, $44   
  .byte $58, $17, $42, $4c   
  .byte $58, $18, $42, $44   

  .byte $66, $09, $42, $1c   
  .byte $66, $0a, $42, $14  
  .byte $6e, $19, $42, $1c   
  .byte $6e, $1a, $42, $14   
 
  .byte $6e, $1b, $42, $34   
  .byte $6e, $1c, $42, $2c

  .byte $66, $0d, $42, $4c   
  .byte $66, $0e, $42, $44
  .byte $6e, $1d, $42, $4c   
  .byte $6e, $1e, $42, $44

  
  .byte $80, $01, $02, $14   ;character facing right
  .byte $88, $11, $02, $14  
  .byte $80, $02, $02, $1c  
  .byte $88, $12, $02, $1c   

  .byte $80, $03, $02, $24   
  .byte $80, $04, $02, $2c   
  .byte $88, $13, $02, $24   
  .byte $88, $14, $02, $2c   

  .byte $80, $05, $02, $34   
  .byte $80, $06, $02, $3c   
  .byte $88, $15, $02, $34   
  .byte $88, $16, $02, $3c   

  .byte $80, $07, $02, $44   
  .byte $80, $08, $02, $4c   
  .byte $88, $17, $02, $44   
  .byte $88, $18, $02, $4c   

  .byte $96, $09, $02, $14   
  .byte $96, $0a, $02, $1c  
  .byte $9e, $19, $02, $14   
  .byte $9e, $1a, $02, $1c   
 
  .byte $9e, $1b, $02, $2c   
  .byte $9e, $1c, $02, $34

  .byte $96, $0d, $02, $44   
  .byte $96, $0e, $02, $4c
  .byte $9e, $1d, $02, $44   
  .byte $9e, $1e, $02, $4c

  

background:
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$14,$14,$14,$14,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$1b,$1b,$1b,$1b,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$14,$14,$14,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$11,$12,$19,$19,$1b,$1b,$1b,$14,$14,$14,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1b,$1b,$1b,$19,$19
	.byte $19,$19,$19,$19,$19,$11,$12,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $14,$14,$14,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $1b,$1b,$1b,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$15,$16,$15,$16,$15,$16,$15,$16
	.byte $15,$16,$15,$16,$15,$16,$15,$16,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$15,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$16,$19,$19,$19,$19,$19,$19,$19
	.byte $17,$17,$18,$18,$18,$18,$18,$1c,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$1d,$18,$18,$17,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$1c,$1d,$1c,$1d,$1c,$1d,$1c,$1d
	.byte $1c,$1d,$1c,$1d,$1c,$1d,$1c,$1d,$18,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$17,$18,$18,$17,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$18,$17,$17,$18,$18,$18,$18,$18,$18,$17,$17
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$17,$17,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$17,$17,$17,$17,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$17,$18,$18,$18,$18,$18,$18,$17,$17,$17,$18
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$17,$17,$18,$18,$18
	.byte $17,$17,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$17,$17,$17,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$17,$17,$18,$18,$18,$17,$17,$17,$17,$17,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$17,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$17,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$18,$17,$17,$17,$17,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$17,$17,$17,$18,$18,$18,$17
	.byte $13,$1a,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$13,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$13,$19,$19,$19,$19,$19,$19,$13,$13,$13
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$13,$13,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$13,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$13,$13,$19,$19,$19,$19,$19,$19,$19,$1a
	.byte $13,$19,$19,$19,$19,$19,$19,$19,$13,$19,$19,$19,$19,$19,$19,$19
	.byte $13,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1a
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19
	.byte $19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$13,$19,$19
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a


attributes:
  .byte $55,$55,$55,$55,$55,$55,$55,$55 ; %01010101 is first palette
  .byte $55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ; %10101010 is second palette 
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA

; Character memory
.segment "CHARS"
.incbin "original.chr"  ; Include your imported .chr file