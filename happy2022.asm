; Happy 2022
; For New Years Disk
; F#READY, 2021-12-31

; v4 - optimised size to 1024 bytes
; v3 - added dli lines, 1119 bytes
; v2 - added wave
; v1 - show text in PM
          
SCR_WIDTH   = 48
SCR_HEIGHT  = 26

BOOL_WIDTH  = SCR_WIDTH+1
BOOL_HEIGHT = SCR_HEIGHT+1

            org $4000
march_font
            ins 'marching.fnt',0,16*8

VDSLST      = $0200
SDMCTL      = $022f
SDLSTL      = $0230
GPRIOR      = $026f
CHBAS       = $02f4

HPOSP0      = $d000
SIZEP0      = $d008
COLPM0      = $d012
COLBK       = $d01a
GRACTL      = $d01d

RANDOM      = $D20A

PMBASE      = $d407
WSYNC       = $d40a
NMIEN       = $d40e

bool_tab_lo = $4400
bool_tab_hi = $4500
scr_tab_lo  = $4600
scr_tab_hi  = $4700

pm_area     = $4800

sinewave    = $6000
pm_sine1    = $6100
pm_sine2    = $6200

bool_mem    = $5000 ; 1 to 2 KB
scr_mem     = $5800 ; 1 to 2 KB
                    
bool_ptr    = $f0
scr_ptr     = $f2
temp_ptr    = $f4
temp_row    = $f6
temp_char   = $f7
last_bool   = $f8
tmp         = $fa
offset_y    = $fc

            org $a000

main
            jsr gen_sine

            lda #<display_list
            sta SDLSTL
            lda #>display_list
            sta SDLSTL+1
            
            lda #7         ; sets VVBLKI
            ldy #<vbi
            ldx #>vbi
            jsr $e45c       ; SETVBV

            lda #$c0
            sta NMIEN
            
            lda #>pm_area
            sta PMBASE
            
            lda #255
            sta GRACTL
            
            ldx #4
setsize
            sta SIZEP0,x
            dex
            bpl setsize
            inx
            stx scr_mem+2

            ldx #7
setcol
            lda coltab,x
            sta 704,x
            dex
            bpl setcol
                        
            lda #35+12      ; enable PM
            sta SDMCTL
            
            lda #1+16
            sta GPRIOR
            
            jsr fill_bool_tab
            jsr fill_scr_tab

            lda #>march_font
            sta CHBAS            

            jsr fill_bool_rand            
            jsr bool_to_scr

loop
            jsr move_it

            jsr get_bool
            sta last_bool

            jsr move_it
                        
            lda last_bool
            jsr set_bool

            jsr convert_cell_nb
            jmp loop

coltab      dta $4a,$5a,$6a,$7a
            dta $00,$04,$00,$8a

; sine generator

gen_sine
            ldy #$3f
            ldx #0

makesine
value_lo    lda #0              ; self-mod
            clc
delta_lo    adc #0              ; self-mod
            sta value_lo+1
value_hi    lda #0              ; self-mod
delta_hi    adc #0              ; self-mod
            sta value_hi+1
 
            sta sinewave+$c0,x
            sta sinewave+$80,y
            eor #$7f
            sta sinewave+$40,x
            sta sinewave+$00,y
 
            lda delta_lo+1
            adc #8
            sta delta_lo+1
            bcc nothi
            inc delta_hi+1
nothi
            inx
            dey
            bpl makesine

; continue with flatten

; make sine flat for small move

flatten_sine
        ldx #0
flat_it
        lda sinewave,x
        lsr
        lsr
        lsr
        adc #16
        sta pm_sine1,x
        adc #48
        sta pm_sine2,x
        inx
        bne flat_it
        rts
        
; render PM stuff

index_tab1  dta 0,16,32,48,64
index_tab2  dta 0,16,32,48

render_pm

; write HAPPY in PM
            
            ldx #0
write_happy
            stx tmp
            lda index_tab1,x
            inc index_tab1,x
            inc index_tab1,x
            inc index_tab1,x

            tax
            lda pm_sine1,x
            sta offset_y
            ldx tmp
            
            lda happy_lo,x
            sta tmp
            lda #$e1
            sta tmp+1
            
            lda happy_pos_lo,x
            clc
            adc offset_y
            sta CHAR_PM
            lda happy_pos_hi,x
            sta CHAR_PM+1
            
            txa
            pha
            
            jsr render_char            
            
            pla
            tax
            inx
            cpx #5
            bne write_happy

; write 2022 in PM
            ldx #0
write_year            
            stx tmp
            lda index_tab2,x
            inc index_tab2,x
            inc index_tab2,x

            tax
            lda pm_sine2,x
            sta offset_y
            ldx tmp

            lda year_lo,x
            sta tmp
            lda #$e0
            sta tmp+1
            
            lda year_pos_lo,x
            clc
            adc offset_y
            sta CHAR_PM
            lda year_pos_hi,x
            sta CHAR_PM+1
            
            txa
            pha
            
            jsr render_char
            
            pla
            tax
            inx
            cpx #4
            bne write_year
            rts

vbi
            lda #<dli0
            sta VDSLST
            lda #>dli0
            sta VDSLST+1

            ldx #7
setpos
            lda pm_postab,x
            sta HPOSP0,x
            dex
            bpl setpos

            jsr render_pm
            
            jmp $e462     ; SYSVBV exit vblank routine

pm_postab
            dta $30,$50,$70,$90
            dta $c8,$c0,$b8,$b0            

LINE_COLOR  = $7c
dli0        pha
            lda #LINE_COLOR
            sta WSYNC
            sta COLBK
            lda #LINE_COLOR-4
            sta WSYNC
            sta COLBK
            lda #0
            sta WSYNC
            sta COLBK
            
            lda #<dli1
            sta VDSLST
            lda #>dli1
            sta VDSLST+1
            
            pla
            rti            

dli1_pm_pos
            dta $40,$60,$80,$a0,0,0,0,0
dli1_pm_col
            dta $9a,$aa,$ba,$ca

dli1         
            pha
            txa
            pha
            
            ldx #7
dli1_setpos
            lda dli1_pm_pos,x
            sta HPOSP0,x
            dex
            bpl dli1_setpos
            
            ldx #3
dli1_setcol            
            lda dli1_pm_col,x
            sta COLPM0,x
            dex
            bpl dli1_setcol
            
            lda #<dli2
            sta VDSLST
            lda #>dli2
            sta VDSLST+1

            pla
            tax
            pla
            rti

dli2        pha
            lda #LINE_COLOR+$30-4
            sta WSYNC
            sta COLBK
            lda #LINE_COLOR+$30
            sta WSYNC
            sta COLBK
            lda #0
            sta WSYNC
            sta COLBK
            
            pla
            rti            
            
render_char
            ldx #0
write_char            
            txa
            lsr
            lsr
            tay
            lda (tmp),y
CHAR_PM     = *+1
            sta $ffff,x
            
            inx
            cpx #32
            bne write_char
            rts
            
move_tab
            dta a(move_up)
            dta a(move_down)
            dta a(move_left)
            dta a(move_right)
            
move_up
            dex
            bne yes_up
            ldx #SCR_HEIGHT-1
yes_up
            rts
move_down
            inx
            cpx #SCR_HEIGHT
            bne yes_down
            ldx #1
yes_down
            rts
            
move_left   
            dey
            cpy #1
            bne yes_left
            ldy #SCR_WIDTH-1
yes_left
            rts

move_right  
            iny
            cpy #SCR_WIDTH
            bne yes_right
            ldy #2
yes_right
            rts
            
move_it
            txa
            pha
            ldx $d20a
            lda $e200,x
            and #3
            asl
            tax
            lda move_tab,x
            sta MOVE_PTR
            lda move_tab+1,x
            sta MOVE_PTR+1
            pla
            tax

MOVE_PTR    = *+1
            jmp $ffff
            
convert_cell_nb

            jsr convert_cell

            dex
            dey
            jsr convert_cell
            
            iny
            jsr convert_cell

            iny
            jsr convert_cell
            
            inx
            jsr convert_cell

            dey
            dey
            jsr convert_cell

            inx
            jsr convert_cell

            iny
            jsr convert_cell

            iny
            jsr convert_cell
            
            dey
            dex

            rts

; get bool at y,x

get_bool
            lda bool_tab_lo,x
            sta temp_ptr
            lda bool_tab_hi,x
            sta temp_ptr+1

            lda (temp_ptr),y
            rts
            
; set bool at y,x (y reg = col, x reg = row)

set_bool
            pha
            lda bool_tab_lo,x
            sta temp_ptr
            lda bool_tab_hi,x
            sta temp_ptr+1

            pla
            sta (temp_ptr),y
            rts

; fill booleans width+1 * height+1 with random values

fill_bool_rand
            ldx #SCR_HEIGHT      ; y pos

fill_bool_cols
            ldy #SCR_WIDTH      ; x pos

            lda bool_tab_lo,x
            sta temp_ptr
            lda bool_tab_hi,x
            sta temp_ptr+1

fill_bool_row
            lda RANDOM
            and #1
            sta (temp_ptr),y
            dey
            bpl fill_bool_row
            
            dex
            bpl fill_bool_cols

            rts
                            
; make bool y-pointer table

fill_bool_tab
            lda #<bool_mem
            sta bool_ptr
            lda #>bool_mem
            sta bool_ptr+1

            ldy #0
fill_bools
            lda bool_ptr
            sta bool_tab_lo,y
            lda bool_ptr+1
            sta bool_tab_hi,y
            
            lda bool_ptr
            clc
            adc #SCR_WIDTH+1
            sta bool_ptr
            lda bool_ptr+1
            adc #0
            sta bool_ptr+1
            
            iny
            cpy #SCR_HEIGHT+1
            bne fill_bools
            
            rts

; fill screen y-pointer table

fill_scr_tab
            lda #<scr_mem
            sta scr_ptr
            lda #>scr_mem
            sta scr_ptr+1

            ldy #0
fill_scrs
            lda scr_ptr
            sta scr_tab_lo,y
            lda scr_ptr+1
            sta scr_tab_hi,y
            
            lda scr_ptr
            clc
            adc #SCR_WIDTH
            sta scr_ptr
            lda scr_ptr+1
            adc #0
            sta scr_ptr+1
            
            iny
            cpy #SCR_HEIGHT
            bne fill_scrs
            
            rts
            
; convert bool matrix to screen

bool_to_scr
            ldx #SCR_HEIGHT-1       ; y = 23

all_rows
            
            ldy #SCR_WIDTH-1        ; x = 39

all_cols
            jsr convert_cell

            dey
            bpl all_cols
                        
            dex
            bpl all_rows

            rts

convert_cell
; screen row same as bool row, but bool row will look one row ahead
            lda scr_tab_lo,x
            sta scr_ptr
            lda scr_tab_hi,x
            sta scr_ptr+1

            lda bool_tab_lo,x
            sta bool_ptr
            lda bool_tab_hi,x
            sta bool_ptr+1

            lda (bool_ptr),y        ; xy 0,0
            sta temp_char

            iny                     ; x = 40
            lda (bool_ptr),y        ; xy 1,0
            clc
            ror
            rol temp_char

            inx                     ; y = 24            
            lda bool_tab_lo,x
            sta bool_ptr
            lda bool_tab_hi,x
            sta bool_ptr+1

            lda (bool_ptr),y        ; xy 1,1
            clc
            ror
            rol temp_char

            dey                     ; x = 39

            lda (bool_ptr),y        ; xy 0,1
            clc
            ror
            rol temp_char
            
            dex                     ; y = 23

; store char from collected bits

            stx temp_row        ; save current row
            ldx temp_char       ; 0..15
            lda lookup_chars,x
            sta (scr_ptr),y
            ldx temp_row
            
            rts

; pattern is rolled in this order
; b3 b2
; b0 b1
; => nibble b3 b2 b1 b1

lookup_chars            

; 10 = /
            dta 0       ; 0000
            dta 3       ; 0001
            dta 4       ; 0010
            dta 11      ; 0011
            dta 2       ; 0100
            dta 10      ; 0101
            dta 14      ; 0110
            dta 5       ; 0111
            dta 1       ; 1000
            dta 13      ; 1001
            dta 9       ; 1010
            dta 6       ; 1011
            dta 12      ; 1100
            dta 8       ; 1101
            dta 7       ; 1110
            dta 15      ; 1111

; $e140 H
; $e108 A
; $e180 P
; $e180 P
; $e1c8 Y

happy_lo
        dta $40,$08,$80,$80,$c8

year_pos_lo
happy_pos_lo
        dta <(pm_area+$200)
        dta <(pm_area+$280)
        dta <(pm_area+$300)
        dta <(pm_area+$380)
        dta <(pm_area+$180)

year_pos_hi
happy_pos_hi
        dta >(pm_area+$200)
        dta >(pm_area+$280)
        dta >(pm_area+$300)
        dta >(pm_area+$380)
        dta >(pm_area+$180)

; $e090 2
; $e080 0
; $e090 2
; $e090 2

TOP_OFFSET2 = 64
year_lo
        dta $90,$80,$90,$90

        .align $400
        
display_list
        dta $70,$50+$80,$20
        dta $42
        dta a(scr_mem)
        dta $02,$02,$02,$02
        dta $02,$02,$02,$02
        dta $02,$02,$02,$02+128
        dta $02,$02,$02,$02
        dta $02,$02,$02,$02
        dta $02,$02,$02,$02
        dta $02,0+128
        dta $41
        dta a(display_list)
            
            run main