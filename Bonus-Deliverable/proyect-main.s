
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
; PLAYER 1
player_x:         .res 1 ; left bottom sprite
player_y:         .res 1 ; left bottom sprite
player_dir:       .res 1 ; 0 is left, 1 is right
pad1:             .res 1 ; will contain information for control 1
frameCount:       .res 1 ; for walking frames, first frame is zero, last frame is 2
timer:            .res 1 ; for the delay in walking frames counts from 1-3
damageCount:      .res 1 ; for the counter of damage
airborne:         .res 1 ; 0 is ground, 1 is on air
jump_count:       .res 1 ; counter to know if jump has reached desired height
player_up:        .res 1 ; 0 is down, 1 is up
player_punching:  .res 1 ; 0 is not punching, 1 is punching
player_active:    .res 1 ; 0 is idle, 1 is active

; PLAYER 2
player_x2:         .res 1 ; left bottom sprite
player_y2:         .res 1 ; left bottom sprite
player_dir2:       .res 1 ; 0 is left, 1 is right
pad2:              .res 1 ; will contain information for control 1
frameCount2:       .res 1 ; for walking frames, first frame is zero, last frame is 2
timer2:            .res 1 ; for the delay in walking frames counts from 1-3
damageCount2:      .res 1 ; for the counter of damage
airborne2:         .res 1 ; 0 is ground, 1 is on air
jump_count2:       .res 1 ; counter to know if jump has reached desired height
player_up2:        .res 1 ; 0 is down, 1 is up
player_punching2:  .res 1 ; 0 is not punching, 1 is punching
player_active2:    .res 1 ; 0 is idle, 1 is active

; Important Registers
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014

; For controllers
CONTROLLER1 = $4016
CONTROLLER2 = $4017
BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN    = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR ; tells the PPU to prepare for a transfer to OAM starting at byte zero
  LDA #$02
  STA OAMDMA ; initiate a high-speed transfer of the 256 bytes from $0200-$02ff into OAM
	LDA #$00

  ; read controller, update tiles and draw player 1
	JSR read_controller1
	JSR update_player1
  JSR draw1

  ; read controller, update tiles and draw player 2
	JSR read_controller2
	JSR update_player2
  JSR draw2

	STA $2005 ; no background scrolling 
	STA $2005
  RTI
.endproc

.proc reset_handler
  SEI
  CLD
  LDX #$00   ; turn off NMIs and disable rendering to the screen during startup
  STX PPUCTRL 
  STX PPUMASK

vblankwait:
  BIT PPUSTATUS ; repeatedly fetches the PPU's status  until it reports that it is ready
  BPL vblankwait

	LDX #$00
	LDA #$ff
clear_oam: ; clear sprites
	STA $0200, X ; set sprite y-positions off the screen
	INX
	INX
	INX
	INX
	BNE clear_oam

vblankwait2:
	BIT PPUSTATUS
	BPL vblankwait2

	; Initialize zero-page values for player 1
	LDA #$08
	STA player_x
	LDA #$af
	STA player_y
  LDA #$00
  STA frameCount
  STA player_dir 
  STA timer
  STA airborne
  STA jump_count
  STA player_punching
  STA player_active

  ; Initialize zero-page values for player 2
	LDA #$e2
	STA player_x2
	LDA #$af
	STA player_y2
  LDA #$00
  STA frameCount2
  STA player_dir2
  STA timer2
  STA airborne2
  STA jump_count2
  STA player_punching2
  STA player_active2

  JMP main
.endproc

.proc main
  ; Set up PPU address to write palette
  LDX PPUSTATUS        ; Load the PPU status register into X
  LDX #$3f             ; Load 0x3f into X (high byte of PPU address)
  STX PPUADDR          ; Store X into PPU address register (high byte)
  LDX #$00             ; Load 0x00 into X (low byte of PPU address)
  STX PPUADDR          ; Store X into PPU address register (low byte)
LoadPalettes:
  ; Load palettes into PPU
  LDA $2002            ; Load PPU status into accumulator to reset the address latch
  LDA #$3f             ; Load 0x3f into accumulator (high byte of PPU address)
  STA $2006            ; Store accumulator into PPU address register (high byte)
  LDA #$00             ; Load 0x00 into accumulator (low byte of PPU address)
  STA $2006            ; Store accumulator into PPU address register (low byte)
  LDX #$00             ; Initialize X register to 0
LoadPalettesLoop:
  LDA palettes, x      ; Load the value at address 'palettes + x' into accumulator
  STA $2007            ; Store the value in the accumulator into PPU data register
  INX                  ; Increment X register
  CPX #$20             ; Compare X with 0x20 (32 in decimal), the number of palette entries
  BNE LoadPalettesLoop ; Branch back to @loop if X is not equal to 0x20
                        

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


vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen/rendering
  STA PPUMASK

forever:
  JMP forever
.endproc


.proc read_controller1
  PHA
  TXA
  PHA
  PHP

  ; write a 1, then a 0, to CONTROLLER1 to latch button states
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1

  LDA #%00000001 ; sort of a counter
  STA pad1

get_buttons:
  LDA CONTROLLER1 ; Read next button's state
  LSR A           ; Shift button state right, into carry flag
  ROL pad1        ; Rotate button state from carry flag
                  ; onto right side of pad1
                  ; and leftmost 0 of pad1 into carry flag
  BCC get_buttons ; Continue until original "1" is in carry flag

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc


.proc draw1
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  ; Drawing the character.
  ; Logic for walking and jumping with respective 
  ; to the direction the character is moving.
  LDX player_dir 
  LDY airborne
  CPY #$00
  BEQ walking_anim

  
  jumping_anim:
    JSR jumping_or_falling
    CPX #$01
    BEQ jumping_right

    jumping_left:
      JSR jump_left
      JMP updating

    jumping_right:
      JSR jump_right
      JMP updating


  walking_anim:
    CPX #$01
    BEQ walk_punch_stand_right

    walk__punch_stand_left:
      LDY player_active
      CPY #$01
      BEQ walking_left
      LDY player_punching
      CPY #$00
      BEQ standing_left

    punching_left:
      JSR punch_left
      JMP updating

    walking_left:
      JSR walk_left
      JMP updating

    standing_left:
      JSR idle_left
      JMP updating


    walk_punch_stand_right:
      LDY player_active
      CPY #$01
      BEQ walking_right
      LDY player_punching
      CPY #$00
      BEQ standing_right

    punching_right:
      JSR punch_right
      JMP updating

    walking_right:
      JSR walk_right
      JMP updating

    standing_right:
      JSR idle_right
    

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


.proc player_dead_right
; write player tile numbers
  LDA #$0b
  STA $0201
  LDA #$0c
  STA $0205
  LDA #$1b
  STA $0209
  LDA #$1c
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


.proc player_dead_left
; write player tile numbers
  LDA #$0c
  STA $0201
  LDA #$0b
  STA $0205
  LDA #$1c
  STA $0209
  LDA #$1b
  STA $020d

  ; write player tile attributes
  LDA #$42 ; mirroring is used
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc


;;------------------- MOVEMENT LOGIC-----------------------;;


.proc update_player1
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

check_a:         ; player jumps
  LDA pad1
  AND #BTN_A
  BEQ check_b    ; checks the zero flag

  up_collision:
    LDX player_y
    CPX #$07     ; coord is (0,7)
    BEQ check_b 

  on_air:
    LDX airborne
    CPX #$01
    BEQ check_b

  JSR move_up

check_b:          ; player punches
  LDA pad1
  AND #BTN_B
  BEQ check_left
  JSR punch

check_left:       ; player walks left
  LDA pad1        ; Load button presses
  AND #BTN_LEFT   ; Filter out all but Left
  BEQ check_right ; If result is zero, left not pressed

  left_collision:
    LDX player_x
    CPX #$00           ; coord is (0,af)
    BEQ check_right

  JSR move_left

check_right:           ; player walks right
  LDA pad1
  AND #BTN_RIGHT
  BEQ check_movement

    right_collision:
    LDX player_x
    CPX #$f0           ; coord is (f0,af)
    BEQ check_movement

  JSR move_right
  
check_movement:        ; player is idle
  LDA pad1
  AND #$43  ; %01000011 buttons that can be used while idle
  BNE done_checking
  LDA #$00
  STA player_active

  LDA pad1
  AND #$40  ; %01000000
  BNE done_checking
  LDA #$00
  STA player_punching

done_checking:
  PLA ; restore registers and return
  TAY 
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc jumping_or_falling
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  LDA player_up
  CMP #$00         ; 0 means its falling
  BEQ inc_y

  dec_y:           ; jumping up
    LDA player_y
    CMP #$07       ; top of screen 
    BEQ go_down
    LDA jump_count
    CMP #$70
    BEQ go_down

    DEC player_y
    INC jump_count
    JMP end

    go_down:
      LDA #$00
      STA player_up
      JMP end

  inc_y:
    JSR check_falling_collision
    LDA jump_count
    CMP #$00
    BEQ end_jump_anim

    INC player_y
    JMP end

    end_jump_anim:
      LDA #$00
      STA jump_count
      STA airborne

  end:
  PLA ; Done with updates, restore registers
  TAY 
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc check_falling_collision ; for platform
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  LDX player_y
  CPX #$af ; ground
  BEQ is_on_ground

  LDX player_y
  CPX #$58 ; cloud height
  BNE end

  LDY player_x
  CPY #$bd ; right cloud corner
  BCS end  ; branches if the carry flag is set (unsigned comparison)
           ; register value is greater or equal to the compared value

  CPY #$37 ; left cloud corner
  BCC end  ; branches if the carry flag is clear (unsigned comparison)
           ; means that the register value is less than the compared value

  is_on_ground:
    LDY #$00
    STY airborne
    STY jump_count

  end:
  PLA ; Done with updates, restore registers
  TAY 
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc move_up
 ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX airborne
  CPX #$01
  BEQ end   ; to prevent double-jumping and such
  LDX #$01
  STX airborne
  STX player_up
  LDX #$00
  STX jump_count

  end:
  PLA ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc move_left
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA airborne
  CMP #$00
  BNE end

  check_edge: ; for platform
    LDA player_y
    CMP #$58
    BNE end 

    ; Storing player direction
    platform_fall:
      LDA player_x
      CMP #$37
      BCS end ; branches if the carry flag is set (unsigned comparison)
              ; register value is greater or equal to the compared value

      set_fall:
        LDY #$01
        STY airborne
        STY jump_count
        LDY #$00
        STY player_up

  end:
    DEC player_x
    LDY #$00
    STY player_dir
    LDY #$01
    STY player_active

  PLA   ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS

.endproc


.proc move_right
   ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA airborne
  CMP #$00
  BNE end

  check_edge: ; for platform
    LDA player_y
    CMP #$58
    BNE end 

    platform_fall:
      LDA player_x
      CMP #$bd
      BCC end ; branches if the carry flag is clear (unsigned comparison)
              ; means that the register value is less than the compared value

      set_fall:
        LDY #$01
        STY airborne
        STY jump_count
        LDY #$00
        STY player_up

  end:
    INC player_x
    LDY #$01
    STY player_dir
    STY player_active

  PLA  ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc punch
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDY #$01
  STY player_punching

  PLA   ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc





; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; 
;;-------------------PLAYER TWO----------------------;;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; 


.proc read_controller2
  PHA
  TXA
  PHA
  PHP

  LDA #$01
  STA CONTROLLER2
  LDA #$00
  STA CONTROLLER2

  LDA #%00000001
  STA pad2

  get_buttons2:
    LDA CONTROLLER2 
    LSR A           
    ROL pad2 
    BCC get_buttons2

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc



.proc draw2
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  ; Drawing the character.
  ; Logic for walking and jumping with respective 
  ; to the direction the character is moving.
  LDX player_dir2 
  LDY airborne2
  CPY #$00
  BEQ walking_anim2

  
  jumping_anim2:
    JSR jumping_or_falling2
    CPX #$01
    BEQ jumping_right2

    jumping_left2:
      JSR jump_left2
      JMP updating2

    jumping_right2:
      JSR jump_right2
      JMP updating2


  walking_anim2:
    CPX #$01
    BEQ walk_punch_stand_right2

    walk__punch_stand_left2:
      LDY player_active2
      CPY #$01
      BEQ walking_left2
      LDY player_punching2
      CPY #$00
      BEQ standing_left2

    punching_left2:
      JSR punch_left2
      JMP updating2

    walking_left2:
      JSR walk_left2
      JMP updating2

    standing_left2:
      JSR idle_left2
      JMP updating2


    walk_punch_stand_right2:
      LDY player_active2
      CPY #$01
      BEQ walking_right2
      LDY player_punching2
      CPY #$00
      BEQ standing_right2

    punching_right2:
      JSR punch_right2
      JMP updating2

    walking_right2:
      JSR walk_right2
      JMP updating2

    standing_right2:
      JSR idle_right2
    

  updating2:
    JSR update_location2


  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc update_location2
  ; store tile locations
  ; top left tile
  LDA player_y2
  STA $0220
  LDA player_x2
  STA $0223

  ; top right tile (x + 8):
  LDA player_y2
  STA $0224
  LDA player_x2
  CLC
  ADC #$08
  STA $0227

  ; bottom left tile (y + 8):
  LDA player_y2
  CLC
  ADC #$08
  STA $0228
  LDA player_x2
  STA $022b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y2
  CLC
  ADC #$08
  STA $022c
  LDA player_x2
  CLC
  ADC #$08
  STA $022f
  RTS
.endproc



;;------------------- DRAWING-----------------------;;

.proc jump_left2
  ; write player tile numbers
  LDA #$0a
  STA $0221
  LDA #$09
  STA $0225
  LDA #$1a
  STA $0229
  LDA #$19
  STA $022d

  ; write player tile attributes
  LDA #$43 ; mirroring is used
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc


.proc jump_right2
  ; write player tile numbers
  LDA #$09
  STA $0221
  LDA #$0a
  STA $0225
  LDA #$19
  STA $0229
  LDA #$1a
  STA $022d

  ; write player tile attributes
  LDA #$03
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc


.proc punch_left2
  ; write player tile numbers
  LDA #$0e
  STA $0221
  LDA #$0d
  STA $0225
  LDA #$1e
  STA $0229
  LDA #$1d
  STA $022d

  ; write player tile attributes
  LDA #$43  ; mirroring is used
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  
  RTS
.endproc


.proc punch_right2
  ; write player tile numbers
  LDA #$0d
  STA $0221
  LDA #$0e
  STA $0225
  LDA #$1d
  STA $0229
  LDA #$1e
  STA $022d

  ; write player tile attributes
  LDA #$03
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc


.proc walk_left2
  ; timer delay for frames
  LDA timer2
  CMP #$03
  BEQ left_cycle2
  INC timer2
  JMP update_location2
  

  left_cycle2:
  ; reset timer
  LDA #$00
  STA timer2

  ; checking which animation should be displayed
  LDX frameCount2
  CPX #$00
  BEQ left_cycle12

  LDX frameCount2
  CPX #$01
  BEQ left_cycle22

  LDX frameCount2
  CPX #$02
  BEQ left_cycle32

  left_cycle12:
    LDA #$04
    STA $0221
    LDA #$03
    STA $0225
    LDA #$14
    STA $0229
    LDA #$13
    STA $022d

    LDA #$43
    STA $0222
    STA $0226
    STA $022a
    STA $022e

    LDX #$01
    STX frameCount2
    JMP update_location2
    
  left_cycle22:
    LDA #$06
    STA $0221
    LDA #$05
    STA $0225
    LDA #$16
    STA $0229
    LDA #$15
    STA $022d
    
    LDA #$43
    STA $0222
    STA $0226
    STA $022a
    STA $022e

    LDX #$02
    STX frameCount2
    JMP update_location2
    
  left_cycle32:
    LDA #$08
    STA $0221
    LDA #$07
    STA $0225
    LDA #$18
    STA $0229
    LDA #$17
    STA $022d

    LDA #$43
    STA $0222
    STA $0226
    STA $022a
    STA $022e

    LDX #$00
    STX frameCount2
    JMP update_location2
  RTS
.endproc


.proc walk_right2
  ; timer delay for frames
  LDA timer2
  CMP #$03
  BEQ right_cycle2
  INC timer2
  JMP update_location2
  
  right_cycle2:
  ; reset timer
  LDA #$00
  STA timer2

  ; checking which animation should be displayed
  LDX frameCount2
  cpx #$00
  beq right_cycle12

  LDX frameCount2
  cpx #$01
  beq right_cycle22

  LDX frameCount2
  cpx #$02
  beq right_cycle32

  right_cycle12:
    LDA #$03
    STA $0221
    LDA #$04
    STA $0225
    LDA #$13
    STA $0229
    LDA #$14
    STA $022d

    LDA #$03
    STA $0222
    STA $0226
    STA $022a
    STA $022e

    LDX #$01
    STX frameCount2
    JMP update_location2
    
  right_cycle22:
    LDA #$05
    STA $0221
    LDA #$06
    STA $0225
    LDA #$15
    STA $0229
    LDA #$16
    STA $022d
    
    LDA #$03
    STA $0222
    STA $0226
    STA $022a
    STA $022e

    LDX #$02
    STX frameCount2
    JMP update_location2
    
  right_cycle32:
    LDA #$07
    STA $0221
    LDA #$08
    STA $0225
    LDA #$17
    STA $0229
    LDA #$18
    STA $022d

    LDA #$03
    STA $0222
    STA $0226
    STA $022a
    STA $022e

    LDX #$00
    STX frameCount2
    JMP update_location2
  RTS
.endproc

.proc idle_left2
  ; write player tile numbers
  LDA #$02
  STA $0221
  LDA #$01
  STA $0225
  LDA #$12
  STA $0229
  LDA #$11
  STA $022d

  ; write player tile attributes
  LDA #$43 ; mirroring is used
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc

.proc idle_right2
  ; write player tile numbers
  LDA #$01
  STA $0221
  LDA #$02
  STA $0225
  LDA #$11
  STA $0229
  LDA #$12
  STA $022d

  ; write player tile attributes
  LDA #$03
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc


.proc player_dead_right2
; write player tile numbers
  LDA #$0b
  STA $0221
  LDA #$0c
  STA $0225
  LDA #$1b
  STA $0229
  LDA #$1c
  STA $022d

  ; write player tile attributes
  LDA #$03
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc


.proc player_dead_left2
; write player tile numbers
  LDA #$0c
  STA $0221
  LDA #$0b
  STA $0225
  LDA #$1c
  STA $0229
  LDA #$1b
  STA $022d

  ; write player tile attributes
  LDA #$43 ; mirroring is used
  STA $0222
  STA $0226
  STA $022a
  STA $022e
  RTS
.endproc


;;------------------- MOVEMENT LOGIC-----------------------;;


.proc update_player2
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

check_a2:         ; player jumps
  LDA pad2
  AND #BTN_A
  BEQ check_b2    ; checks the zero flag

  up_collision2:
    LDX player_y2
    CPX #$07     ; coord is (0,7)
    BEQ check_b2 

  on_air2:
    LDX airborne2
    CPX #$01
    BEQ check_b2

  JSR move_up2

check_b2:          ; player punches
  LDA pad2
  AND #BTN_B
  BEQ check_left2
  JSR punch2

check_left2:       ; player walks left
  LDA pad2         ; Load button presses
  AND #BTN_LEFT    ; Filter out all but Left
  BEQ check_right2 ; If result is zero, left not pressed

  left_collision2:
    LDX player_x2
    CPX #$00           ; coord is (0,af)
    BEQ check_right2

  JSR move_left2

check_right2:           ; player walks right
  LDA pad2
  AND #BTN_RIGHT
  BEQ check_movement2

    right_collision2:
    LDX player_x2
    CPX #$f0           ; coord is (f0,af)
    BEQ check_movement2

  JSR move_right2
  
check_movement2:        ; player is idle
  LDA pad2
  AND #$43  ; %01000011 buttons that can be used while idle
  BNE done_checking
  LDA #$00
  STA player_active2

  LDA pad2
  AND #$40  ; %01000000
  BNE done_checking
  LDA #$00
  STA player_punching2

done_checking:
  PLA ; restore registers and return
  TAY 
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc jumping_or_falling2
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  LDA player_up2
  CMP #$00         ; 0 means its falling
  BEQ inc_y2

  dec_y2:           ; jumping up
    LDA player_y2
    CMP #$07       ; top of screen 
    BEQ go_down2
    LDA jump_count2
    CMP #$70
    BEQ go_down2

    DEC player_y2
    INC jump_count2
    JMP end

    go_down2:
      LDA #$00
      STA player_up2
      JMP end

  inc_y2:
    JSR check_falling_collision2
    LDA jump_count2
    CMP #$00
    BEQ end_jump_anim2

    INC player_y2
    JMP end

    end_jump_anim2:
      LDA #$00
      STA jump_count2
      STA airborne2

  end:
  PLA ; Done with updates, restore registers
  TAY 
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc check_falling_collision2 ; for platform
  PHP  ; Start by saving registers
  PHA  
  TXA
  PHA
  TYA
  PHA

  LDX player_y2
  CPX #$af ; ground
  BEQ is_on_ground2

  LDX player_y2
  CPX #$58 ; cloud height
  BNE end

  LDY player_x2
  CPY #$bd ; right cloud corner
  BCS end  ; branches if the carry flag is set (unsigned comparison)
           ; register value is greater or equal to the compared value

  CPY #$37 ; left cloud corner
  BCC end  ; branches if the carry flag is clear (unsigned comparison)
           ; means that the register value is less than the compared value

  is_on_ground2:
    LDY #$00
    STY airborne2
    STY jump_count2

  end:
  PLA ; Done with updates, restore registers
  TAY 
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc move_up2
 ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX airborne2
  CPX #$01
  BEQ end   ; to prevent double-jumping and such
  LDX #$01
  STX airborne2
  STX player_up2
  LDX #$00
  STX jump_count2

  end:
  PLA ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc move_left2
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA airborne2
  CMP #$00
  BNE end

  check_edge2: ; for platform
    LDA player_y2
    CMP #$58
    BNE end 

    ; Storing player direction
    platform_fall2:
      LDA player_x2
      CMP #$37
      BCS end ; branches if the carry flag is set (unsigned comparison)
              ; register value is greater or equal to the compared value

      set_fall2:
        LDY #$01
        STY airborne2
        STY jump_count2
        LDY #$00
        STY player_up2

  end:
    DEC player_x2
    LDY #$00
    STY player_dir2
    LDY #$01
    STY player_active2

  PLA   ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS

.endproc


.proc move_right2
   ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA airborne2
  CMP #$00
  BNE end

  check_edge2: ; for platform
    LDA player_y2
    CMP #$58
    BNE end 

    platform_fall2:
      LDA player_x2
      CMP #$bd
      BCC end ; branches if the carry flag is clear (unsigned comparison)
              ; means that the register value is less than the compared value

      set_fall2:
        LDY #$01
        STY airborne2
        STY jump_count2
        LDY #$00
        STY player_up2

  end:
    INC player_x2
    LDY #$01
    STY player_dir2
    STY player_active2

  PLA  ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc punch2
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDY #$01
  STY player_punching2

  PLA   ; restore registers and return
  TAY
  PLA
  TAX
  PLA
  PLP
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
  .byte $0f, $09, $36, $0f ;P3


background: ; 32x30 bytes = 960 bytes, each tile is 8x8
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

  ; At the end of each nametable is a 64-byte region for attribute table
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a


; Character memory
.segment "CHARS"
.incbin "original.chr"  ; Include imported .chr file