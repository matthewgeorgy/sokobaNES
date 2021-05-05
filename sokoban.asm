.include "constants.inc"
xpos = $00
ypos = $01
buttons = $02
box_x = $03
box_y = $04

; We need a way to check collisions for several boxes at any given time. To do
; this we will use 'box_{x|y}' as a temp location for dealing with box
; locations. There are also the subroutines (hit_box_left/right..) that are
; used to actually verify a collision. The process for collision detection will
; then look something like this:
;
; 1. Load a box's coordinates into box_x/y
; 2. Execute the collision subroutines using that box_x/y
; 3. Load the updated box_x/y coords into the orignal location for that box
; 4. Repeat for any boxes in the current puzzle

.segment "HEADER"
	.byte "NES"
	.byte $1a
	.byte $02 ; 2 * 16KB PRG ROM
	.byte $01 ; 1 * 8KB CHR ROM
	.byte %00000000 ; mapper and mirroring
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00, $00, $00, $00, $00 ; filler bytes


.segment "ZEROPAGE" ; LSB 0 - FF
	world: .res 2


.segment "STARTUP"
.proc reset_handler
		sei ; Disables all interrupts
		cld ; disable decimal mode

		; Disable sound IRQ
		ldx #$40
		stx $4017

		; Initialize the stack register
		ldx #$FF
		txs
		inx ; #$FF + 1 => #$00

		; Zero out the PPU registers
		stx PPUSTATUS
		stx PPUMASK
		stx $4010

	:
		bit PPUSTATUS
		bpl :-
		txa

	clear_mem:
		sta $0000, x ; $0000 => $00FF
		sta $0100, x ; $0100 => $01FF
		sta $0300, x
		sta $0400, x
		sta $0500, x
		sta $0600, x
		sta $0700, x
		lda #$FF
		sta $0200, x ; $0200 => $02FF
		lda #$00
		inx
		bne clear_mem    
	; wait for vblank
	:
		bit PPUSTATUS
		bpl :-

		lda #$02
		sta OAMDMA
		nop
.endproc ; reset_handler

.segment "CODE"
.proc main
		; init PPU addr
		lda #$3F
		sta PPUADDR
		lda #$00
		sta PPUADDR

		ldx #$00
	load_palettes:
		lda palette_data, x
		sta PPUDATA
		inx
		cpx #$20
		bne load_palettes

		; initialize 'world' variable to point to world data
		lda #<world_data ; low byte
		sta world
		lda #>world_data ; high byte
		sta world + 1

		; setup address in PPU for nametable data
		bit PPUSTATUS ; reset PPU latch
		lda #$20
		sta PPUADDR
		lda #$00
		sta PPUADDR

		ldx #$00
		ldy #$00
	load_world:
		lda (world), y
		sta PPUDATA
		iny
		cpx #$03
		bne :+
		cpy #$C0
		beq done_loading_world
	:
		cpy #$00
		bne load_world
		inx
		inc world + 1
		jmp load_world

	done_loading_world:
	
		ldx #$00
	set_attributes:
		lda #$55
		sta PPUDATA
		inx
		cpx #$40
		bne set_attributes

		ldx #$00
		ldy #$00
	load_sprites:
		lda sprite_data, x
		sta $0200, x
		inx
		cpx #$10
		bne load_sprites
	
		; Enable interrupts
		cli

		lda #%10010000
		sta PPUCTRL
		lda #%00011110
		sta PPUMASK

	loop:
		jmp loop
.endproc ; main

; ------------------- Subroutines ----------------------------

.proc update_sprites
		lda #$00
		sta OAMADDR
		lda #$02
		sta OAMDMA

		rts
.endproc ; update_sprites

.proc read_controller
	; config + read data
	config_controller:
		; latch state
		lda #$01
		sta CONTR1
		ldx #$00
		stx CONTR1

	read_buttons:
		lda $4016
		lsr a
		ror buttons			; RLDUsSBA
		inx
		cpx #$08
		bne read_buttons

	; check each button
	check_right:
		lda #$80
		and buttons
		beq check_left
		ldx #$00
		jsr move_right

	check_left:
		lda #$40
		and buttons
		beq check_down
		ldx #$00
		jsr move_left

	check_down:
		lda #$20
		and buttons
		beq check_up
		ldx #$00
		jsr move_down

	check_up:
		lda #$10
		and buttons
		beq done
		ldx #$00
		jsr move_up

	done:
		rts	
.endproc ; read_controller

.proc move_right
		inc xpos
		lda xpos
		sta $0203
		; box 1
		jsr load_box_1
		jsr hit_box_right
		jsr store_box_1
		; box 2
		jsr load_box_2
		jsr hit_box_right
		jsr store_box_2

		rts
.endproc ; move_right

.proc move_left
		dec xpos
		lda xpos
		sta $0203
		; box 1
		jsr load_box_1
		jsr hit_box_left
		jsr store_box_1
		; box 2
		jsr load_box_2
		jsr hit_box_left
		jsr store_box_2

		rts
.endproc ; move_left

.proc move_down
		inc ypos
		lda ypos
		sta $0200
		; box 1
		jsr load_box_1
		jsr hit_box_down
		jsr store_box_1
		; box 2
		jsr load_box_2
		jsr hit_box_down
		jsr store_box_2

		rts
.endproc ; move_down

.proc move_up
		dec ypos
		lda ypos
		sta $0200
		; box 1
		jsr load_box_1
		jsr hit_box_up
		jsr store_box_1
		; box 2
		jsr load_box_2
		jsr hit_box_up
		jsr store_box_2

		rts
.endproc ; move_down

.proc hit_box_right
		; adjust for coord difference
		lda xpos
		clc
		adc #$08
		; check x for collision
		cmp box_x
		bne :+
		; check y1 for collision		
		jsr check_y1
		bcc :+
		; check y2 for collision		
		jsr check_y2
		bcc :+
		; move box to the right
		lda box_x
		clc
		adc #$01
		sta box_x
	:
		rts
.endproc ; hit_box_right

.proc hit_box_left
		; adjust for coord difference
		lda xpos
		clc
		sbc #$07
		; check x for collision
		cmp box_x
		bne :+
		; check y1 for collision		
		jsr check_y1
		bcc :+
		; check y2 for collision		
		jsr check_y2
		bcc :+
		; move box to the left
		lda box_x
		clc
		sbc #$01
		sta box_x
	:
		rts
.endproc ; hit_box_left

.proc hit_box_down
		lda ypos
		clc
		adc #$08
		; check y for collision
		cmp box_y
		bne :+
		; check x1
		jsr check_x1
		bcc :+
		; check x2
		jsr check_x2
		bcc :+
		; move down
		lda box_y
		clc
		adc #$01
		sta box_y
	:
		rts
.endproc ; hit_box_down

.proc hit_box_up
		lda box_y
		clc
		adc #$08
		; check y for collision
		cmp ypos
		bne :+
		; check x1
		jsr check_x1
		bcc :+
		; check x2
		jsr check_x2
		bcc :+
		; move up
		lda box_y
		clc
		sbc #$01
		sta box_y
	:
		rts
.endproc ; hot_bix_up

.proc load_box_1
		lda $0207
		sta box_x
		lda $0204
		sta box_y

		rts
.endproc ; load_box_1

.proc store_box_1
		lda box_x
		sta $0207
		lda box_y
		sta $0204

		rts
.endproc ; store_box_1

.proc load_box_2
		lda $020B
		sta box_x
		lda $0208
		sta box_y

		rts
.endproc ; load_box_1

.proc store_box_2
		lda box_x
		sta $020B
		lda box_y
		sta $0208

		rts
.endproc ; store_box_2

.proc check_x1
		lda box_x
		clc
		adc #$08
		cmp xpos

		rts
.endproc ; check_x1

.proc check_x2
		lda xpos
		clc
		adc #$08
		cmp box_x

		rts
.endproc ; check_x2

.proc check_y1
		lda box_y
		clc
		adc #$08
		cmp ypos

		rts
.endproc ; check_y1

.proc check_y2
		lda ypos
		clc
		adc #$08
		cmp box_y

		rts
.endproc ; check_y2

; -------------- Interrupts --------------------------

.proc nmi_handler
		jsr update_sprites
		jsr read_controller
		rti
.endproc

.proc irq_handler
		rti
.endproc

; ------------- Data ----------------------------

palette_data:
	; background palettte data
	.byte $22, $29, $1A, $0F ; palette 0
	.byte $22, $36, $17, $0F ; palette 1
	.byte $22, $30, $21, $0F ; palette 2
	.byte $22, $27, $17, $0F ; palette 3
	; sprite palette data
	.byte $22, $16, $27, $18 ; palette 0 (4)
	.byte $22, $17, $07, $0F ; palette 1 (5)
	.byte $22, $16, $30, $27 ; palette 2 (6)
	.byte $22, $0F, $36, $17 ; palette 3 (7)

sprite_data:
	.byte $00, $04, $00, $00 ; player
	.byte $50, $0A, $01, $50
	.byte $60, $0A, $01, $60
	.byte $80, $0A, $01, $80

world_data:
	.incbin "world.nam"


.segment "VECTORS"
	.addr nmi_handler, reset_handler, irq_handler


.segment  "CHARS"
	.incbin "sokoban.chr"
