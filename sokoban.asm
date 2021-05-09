.include "constants.inc"
xpos = $10
ypos = $11
buttons = $12
box_x = $13
box_y = $14
temp = $15

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
        cpx #$20
        bne load_sprites
    
    init_data:
        ; player data
        lda #$4A
        sta xpos
        sta $0203
        sta ypos
        sta $0200
    
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
        ror buttons         ; RLDUsSBA
        inx
        cpx #$08
        bne read_buttons

    ; check each button
    check_right:
        lda #$80
        and buttons
        beq check_left
        jsr move_right

    check_left:
        lda #$40
        and buttons
        beq check_down
        jsr move_left

    check_down:
        lda #$20
        and buttons
        beq check_up
        jsr move_down

    check_up:
        lda #$10
        and buttons
        beq done
        jsr move_up

    done:
        rts 
.endproc ; read_controller

; TODO: We can improve these move_XXX routines by getting rid of redundant loads
; (ie. when something is already loaded) and doing transfers from A to another
; register (and vice-versa) when applicable.

.proc move_right
        inc xpos
        lda xpos
        sta $0203
        ; wall collision detection  

        ; top right
        clc
        adc #$07
        tax
        lda ypos
        clc
        adc #$03
        tay
        jsr check_collide
        bne collide

        ; bottom right
        lda xpos
        clc
        adc #$07
        tax
        lda ypos
        clc
        adc #$0A
        tay
        jsr check_collide
        bne collide

        ; box 1
        jsr load_box_1
        jsr hit_box_right
        jsr store_box_1

        ; box 2
        jsr load_box_2
        jsr hit_box_right
        jsr store_box_2

        ; box 3
        jsr load_box_3
        jsr hit_box_right
        jsr store_box_3
        jmp done
    collide:
        dec xpos
        lda xpos
        sta $0203
    done:
        rts
.endproc ; move_right

.proc move_left
        dec xpos
        lda xpos
        sta $0203
        ; wall collision detection

        ; top left
        tax
        lda ypos
        clc
        adc #$03
        tay
        jsr check_collide
        bne collide

        ; bottom left

        lda ypos
        clc
        adc #$0A
        tay
        jsr check_collide
        bne collide

        ; box 1
        jsr load_box_1
        jsr hit_box_left
        jsr store_box_1

        ; box 2
        jsr load_box_2
        jsr hit_box_left
        jsr store_box_2

        ; box 3
        jsr load_box_3
        jsr hit_box_left
        jsr store_box_3
        jmp done
    collide:
        inc xpos
        lda xpos
        sta $0203
    done:
        rts
.endproc ; move_left

.proc move_down
        inc ypos
        lda ypos
        sta $0200
        ; wall collision detection

        ; bottom left
        ldx xpos
        clc
        adc #$0A
        tay
        jsr check_collide
        bne collide

        ; bottom right
        lda xpos
        clc
        adc #$07
        tax
        lda ypos
        clc
        adc #$0A
        tay
        jsr check_collide
        bne collide

        ; box 1
        jsr load_box_1
        jsr hit_box_down
        jsr store_box_1

        ; box 2
        jsr load_box_2
        jsr hit_box_down
        jsr store_box_2

        ; box 3
        jsr load_box_3
        jsr hit_box_down
        jsr store_box_3
        jmp done
    collide:
        dec ypos
        lda ypos
        sta $0200
    done:
        rts
.endproc ; move_down

.proc move_up
        dec ypos
        lda ypos
        sta $0200
        ; wall collision detection

        ; top left
        ldx xpos
        clc
        adc #$03
        tay
        jsr check_collide
        bne collide

        ; top right
        lda xpos
        clc
        adc #$07
        tax
        lda ypos
        clc
        adc #$03
        tay
        jsr check_collide
        bne collide
        
        ; box 1
        jsr load_box_1
        jsr hit_box_up
        jsr store_box_1

        ; box 2
        jsr load_box_2
        jsr hit_box_up
        jsr store_box_2

        ; box 3
        jsr load_box_3
        jsr hit_box_up
        jsr store_box_3
        jmp done
    collide:
        inc ypos
        lda ypos
        sta $0200

    done:
        rts
.endproc ; move_down

.proc check_collide
        txa     ; x / 64
        lsr
        lsr
        lsr
        lsr
        lsr
        lsr
        sta temp
        tya     ; y/8
        lsr
        lsr
        lsr
        asl     ; * 4
        asl
        clc
        adc temp
        tay     ; byte index

        txa     ; x / 8
        lsr
        lsr
        lsr
        and #%0111
        tax ; bitmask index

        lda collision_map, y
        and bit_mask, x     ; adjusts 0 flag depending on whether we collided
                            ; beq = not collide, bne = collide
        rts
.endproc ; check_collide

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

.proc load_box_3
        lda $020F
        sta box_x
        lda $020C
        sta box_y

        rts
.endproc ; load_box_3

.proc store_box_3
        lda box_x
        sta $020F
        lda box_y
        sta $020C

        rts
.endproc ; store_box_3

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

; TODO: the next big routine here (probably) will be a routien that goes
; through each box/boulder and checks it's position, and then compares that
; position to the position of the markers (aka, where the boulders need to be
; to complete the puzzle). We'll then put this routine into our nmi_handler
; to update the boxes/boulders that are in the correct position, such as by
; giving them a different color.

.proc check_box1 ; hole 1 (top left)
        lda $0207
        ; left bound
        cmp #$50        ; left bound of hole 1
        bcc :+
        ; right bound
        cmp #$60        ; right bound of hole 1
        bcs :+

        lda $0204
        clc
        adc #$03
        ; upper bound
        cmp #$48        ; upper bound of hole 1
        bcc :+

        ; lower bound
        cmp #$50        ; lower bound of hole 1
        bcs :+

        ; change color
        lda #$00
        sta $0206
        jmp done
    :
        ; set default
        lda #$01
        sta $0206

    done:   
        rts
.endproc  ; check_box1

.proc check_box2 ; hole 2 (right)
        lda $020B
        ; left bound
        cmp #$98        ; left bound of hole 2
        bcc :+
        ; right bound
        cmp #$A8        ; right bound of hole 2
        bcs :+

        lda $0208
        clc
        adc #$03
        ; upper bound
        cmp #$68        ; upper bound of hole 2
        bcc :+

        ; lower bound
        cmp #$70        ; lower bound of hole 2
        bcs :+

        ; change color
        lda #$00
        sta $020A
        jmp done
    :
        ; set default
        lda #$01
        sta $020A

    done:   
        rts
.endproc  ; check_box2

.proc check_box3 ; hole 3 (right)
        lda $020F
        ; left bound
        cmp #$50        ; left bound of hole 3
        bcc :+
        ; right bound
        cmp #$60        ; right bound of hole 3
        bcs :+

        lda $020C
        clc
        adc #$03
        ; upper bound
        cmp #$90        ; upper bound of hole 3
        bcc :+

        ; lower bound
        cmp #$98        ; lower bound of hole 3
        bcs :+

        ; change color
        lda #$00
        sta $020E
        jmp done
    :
        ; set default
        lda #$01
        sta $020E

    done:   
        rts
.endproc  ; check_box2

.proc check_boxes
        jsr check_box1
        jsr check_box2
        jsr check_box3

        rts
.endproc ; check_boxes

.proc update_door
        lda #$00
        cmp $0206
        bne :+
        cmp $020A
        bne :+
        cmp $020E
        bne :+
        lda #$02
        sta $0212
        sta $0216
        sta $021A
        sta $021E

    :   
        rts
.endproc ; update_door

; -------------- Interrupts --------------------------

.proc nmi_handler
        jsr read_controller
        jsr update_sprites
        jsr check_boxes
        jsr update_door

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
    .byte $22, $16, $30, $27 ; palette 0 (6)
    .byte $22, $17, $07, $0F ; palette 1 (5)
    .byte $22, $16, $27, $18 ; palette 2 (4)
    .byte $22, $0F, $36, $17 ; palette 3 (7)

sprite_data:
    .byte $00, $0B, $00, $00 ; player   $0200
    .byte $60, $0A, $01, $60 ; box 1    $0204
    .byte $60, $0A, $01, $70 ; box 2    $0208
    .byte $60, $0A, $01, $80 ; box 3    $020C
    .byte $25, $03, $22, $70 ; door 1   $0210
    .byte $25, $03, $22, $78 ; door 2   $0214
    .byte $25, $03, $22, $80 ; door 3   $0218
    .byte $25, $03, $22, $88 ; door 4   $021C

world_data:
    .incbin "world.nam"

collision_map:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000001, %11111111, %11111111, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000

    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000

    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %00000000, %00000000, %10000000
    .byte %00000001, %11111111, %11111111, %10000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000

bit_mask:
    .byte %10000000
    .byte %01000000
    .byte %00100000
    .byte %00010000
    .byte %00001000
    .byte %00000100
    .byte %00000010
    .byte %00000001


.segment "VECTORS"
    .addr nmi_handler, reset_handler, irq_handler


.segment  "CHARS"
    .incbin "sokoban.chr"
