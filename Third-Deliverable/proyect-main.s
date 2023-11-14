.segment "HEADER"
.byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks
.byte %00000000  ; Horizontal mirroring, no save RAM, no mapper
.byte %00000000  ; No special-case flags set, no mapper
.byte $00        ; No PRG-RAM present
.byte $00        ; NTSC format


.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler


.segment "ZEROPAGE"
player_x:         .res 1 ; left bottom sprite
player_y:         .res 1 ; left bottom sprite
player_dir:       .res 1 ; 0 is left, 1 is right
frameCount:       .res 1 ; for walking frames, first frame is zero, last frame is 2
timer:            .res 1 ; for the delay in walking frames counts from 1-3

; Important Registers
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
	LDA #$00

  ; draw character
  JSR draw

	STA $2005
	STA $2005
  RTI
.endproc

.proc reset_handler
  SEI
  CLD
  LDX #$00
  STX PPUCTRL
  STX PPUMASK

vblankwait:
  BIT PPUSTATUS
  BPL vblankwait

	LDX #$00
	LDA #$ff
clear_oam:
	STA $0200,X ; set sprite y-positions off the screen
	INX
	INX
	INX
	INX
	BNE clear_oam

vblankwait2:
	BIT PPUSTATUS
	BPL vblankwait2

	; Initialize zero-page values
	LDA #$08
	STA player_x
	LDA #$af
	STA player_y
  LDA #$00
  STA frameCount
  STA player_dir 
  STA timer
  

  JMP main
.endproc

.proc main
  ; Set up PPU address to write palette
  LDX PPUSTATUS        ; Load the PPU status register into X
  LDX #$3f             ; Load 0x3f into X (high byte of PPU address)
  STX PPUADDR          ; Store X into PPU address register (high byte)
  LDX #$00             ; Load 0x00 into X (low byte of PPU address)
  STX PPUADDR          ; Store X into PPU address register (low byte)
load_palettes:
  ; Load palettes into PPU
  lda $2002            ; Load PPU status into accumulator to reset the address latch
  lda #$3f             ; Load 0x3f into accumulator (high byte of PPU address)
  sta $2006            ; Store accumulator into PPU address register (high byte)
  lda #$00             ; Load 0x00 into accumulator (low byte of PPU address)
  sta $2006            ; Store accumulator into PPU address register (low byte)
  ldx #$00             ; Initialize X register to 0
@loop:
  lda palettes, x      ; Load the value at address 'palettes + x' into accumulator
  sta $2007            ; Store the value in the accumulator into PPU data register
  inx                  ; Increment X register
  cpx #$20             ; Compare X with 0x20 (32 in decimal), the number of palette entries
  bne @loop            ; Branch back to @loop if X is not equal to 0x20



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex 
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
  LDX #$00                ; reset counter
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
LoadBackgroundLoop4:      ; Do this for the leftover bytes till 960 is reached
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
  LDA attributes, x     ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever
.endproc



.proc draw
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  ; Drawing the character.
  ; Logic for walking and jumping with respective 
  ; to the direction the character is moving.
 

  walking_anim:

    ; walking_left:
    ;   JSR walk_left
    ;   JMP updating

    
    walking_right:
      JSR walk_right
      JMP updating    

  updating:
    JSR update_location


  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc update_location
  ; store tile locations
  ; top left tile
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f
  
  RTS
.endproc


;;------------------- DRAWING-----------------------;;

.proc jump_left
  ; write player tile numbers
  LDA #$0a
  STA $0201
  LDA #$09
  STA $0205
  LDA #$1a
  STA $0209
  LDA #$19
  STA $020d

  ; write player tile attributes
  LDA #$42 ; mirroring is used
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc


.proc jump_right
  ; write player tile numbers
  LDA #$09
  STA $0201
  LDA #$0a
  STA $0205
  LDA #$19
  STA $0209
  LDA #$1a
  STA $020d

  ; write player tile attributes
  ; use palette 2
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc


.proc punch_left
  ; write player tile numbers
  LDA #$0e
  STA $0201
  LDA #$0d
  STA $0205
  LDA #$1e
  STA $0209
  LDA #$1d
  STA $020d

  ; write player tile attributes
  LDA #$42  ; mirroring is used
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  
  RTS
.endproc


.proc punch_right
  ; write player tile numbers
  LDA #$0d
  STA $0201
  LDA #$0e
  STA $0205
  LDA #$1d
  STA $0209
  LDA #$1e
  STA $020d

  ; write player tile attributes
  ; use palette 2
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc


.proc walk_left
  ; timer delay for frames
  LDA timer
  CMP #$03
  BEQ left_cycle
  INC timer
  JMP update_location
  

  left_cycle:
  ; reset timer
  LDA #$00
  STA timer

  ; checking which animation should be displayed
  LDX frameCount
  CPX #$00
  BEQ left_cycle1

  LDX frameCount
  CPX #$01
  BEQ left_cycle2

  LDX frameCount
  CPX #$02
  BEQ left_cycle3

  left_cycle1:
    LDA #$04
    STA $0201
    LDA #$03
    STA $0205
    LDA #$14
    STA $0209
    LDA #$13
    STA $020d

    LDA #$42
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$01
    STX frameCount
    JMP update_location
    
  left_cycle2:
    LDA #$06
    STA $0201
    LDA #$05
    STA $0205
    LDA #$16
    STA $0209
    LDA #$15
    STA $020d
    
    LDA #$42
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$02
    STX frameCount
    JMP update_location
    
  left_cycle3:
    LDA #$08
    STA $0201
    LDA #$07
    STA $0205
    LDA #$18
    STA $0209
    LDA #$17
    STA $020d

    LDA #$42
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$00
    STX frameCount
    JMP update_location
  RTS
.endproc


.proc walk_right
  ; timer delay for frames
  LDA timer
  CMP #$03
  BEQ right_cycle
  INC timer
  JMP update_location
  
  right_cycle:
  ; reset timer
  LDA #$00
  STA timer

  ; checking which animation should be displayed
  LDX frameCount
  cpx #$00
  beq right_cycle1

  LDX frameCount
  cpx #$01
  beq right_cycle2

  LDX frameCount
  cpx #$02
  beq right_cycle3

  right_cycle1:
    LDA #$03
    STA $0201
    LDA #$04
    STA $0205
    LDA #$13
    STA $0209
    LDA #$14
    STA $020d

    LDA #$02
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$01
    STX frameCount
    JMP update_location
    
  right_cycle2:
    LDA #$05
    STA $0201
    LDA #$06
    STA $0205
    LDA #$15
    STA $0209
    LDA #$16
    STA $020d
    
    LDA #$02
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$02
    STX frameCount
    JMP update_location
    
  right_cycle3:
    LDA #$07
    STA $0201
    LDA #$08
    STA $0205
    LDA #$17
    STA $0209
    LDA #$18
    STA $020d

    LDA #$02
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$00
    STX frameCount
    JMP update_location
  RTS
.endproc


.proc idle_left
  ; write player tile numbers
  LDA #$02
  STA $0201
  LDA #$01
  STA $0205
  LDA #$12
  STA $0209
  LDA #$11
  STA $020d

  ; write player tile attributes
  LDA #$42 ; mirroring is used
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc


.proc idle_right
  ; write player tile numbers
  LDA #$01
  STA $0201
  LDA #$02
  STA $0205
  LDA #$11
  STA $0209
  LDA #$12
  STA $020d

  ; write player tile attributes
  ; use palette 2
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc




.segment "RODATA"
palettes:
  ; Background Palette
  .byte $0f, $00, $20, $10 ;P0
  .byte $0f, $01, $21, $30 ;P1
  .byte $0f, $07, $36, $18 ;P2
  .byte $0f, $15, $15, $15 ;P3

  ; Sprite Palette
  .byte $0f, $00, $20, $10 ;P0
  .byte $0f, $01, $21, $30 ;P1
  .byte $0f, $07, $36, $18 ;P2
  .byte $0f, $15, $15, $15 ;P3


sprites:
       ; Y  tile  atrr  X
  .byte $af, $01, $42, $10   ; character facing left
  .byte $af, $02, $42, $08   ; atrr is %01000010 to flip horizontal and use palette 2
  .byte $b7, $11, $42, $10  
  .byte $b7, $12, $42, $08   


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
.incbin "original.chr"  ; Include imported .chr file