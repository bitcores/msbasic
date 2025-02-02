.feature org_per_seg
.zeropage

.org ZP_START5
mseread:
    .res 4
kbread:
    .res 4
kbreadp:
    .res 1
fkbtemp:
    .res 1
kbdetect:
    .res 1
kbinput:
    .res 1
mseinput:
    .res 1
READY:
    .res 1
VSCROLL:
    .res 2
VSCROLLY:
    .res 1
YSAV:
    .res 1
YOUT:
    .res 1
ROW:
    .res 1

.segment "CODE"

.export NES_RESET
; prepare the NES for starting MSBASIC
NES_RESET:      SEI
                CLD             ; Clear decimal arithmetic mode.
                LDX #$40        
                STX $4017       ; disable APU frame IRQ
                LDX #$FF        ; Set up stack
                TXS             ; Set stack pointer to $FF
                INX
                STX PPUCTRL     ; disable NMI
                STX PPUMASK     ; disable rendering
                STX $4010       ; disable DMC IRQs
                
;; first wait for vblank to make sure PPU is ready
                BIT PPUSTATUS
                : BIT PPUSTATUS
                BPL :-

clear_ram:      LDA #$00
                STA $0000,X
                STA $0100,X
                STA $0300,X
                STA $0400,X
                STA $0500,X
                STA $0600,X
                STA $0700,X
                LDA #$FE
                STA $0200,X
                INX
                BNE clear_ram

;; second wait for vblank, PPU is ready after this
                : BIT PPUSTATUS
                BPL :-

;; load palettes within vblank to hide visible stripes
load_palettes:  LDA #$3F
                STA PPUADDR
                LDA #$00
                STA PPUADDR
                TAX
                : LDA PALETTES,X
                STA PPUDATA
                INX
                CPX #$20
                BNE :-

;; clear out the nametable/attribute RAM
                BIT PPUSTATUS
                LDA #$20
                STA PPUADDR
                LDA #$00
                STA PPUADDR
                LDY #$04
                : LDX #$00
                : STA PPUDATA
                INX
                BNE :-
                DEY
                BNE :--

;; load CHR-RAM data
                LDA #<TILEDATA
                STA $00    ; these will be initialized later
                LDA #>TILEDATA
                STA $01    ; so I can make use of them here
                LDY #$00
                STY PPUMASK
                STY PPUADDR
                STY PPUADDR
                .ifdef CONFIG_1K_CHR
                LDX #$04        ; store up to 4x 256B pages
                .else
                LDX #$10        ; store up to 16x 256B pages
                .endif
                : LDA ($00),Y
                STA PPUDATA
                INY
                BNE :-
                INC $01    ; without causing problems
                DEX
                BNE :-

;; finally enable rendering
                LDA #$00        ; set scroll
                STA PPUSCROLL
                STA PPUSCROLL
                LDA #%10000000  ; Enable NMI
                STA PPUCTRL

;; initialize keyboard
                JSR KBINIT
                
                LDY #$20        ; set the VSCROLL start       
                STY VSCROLL+1
                INY             ; start on second line for overscan reasons
                STY VSCROLL

                JMP COLD_START


;; ------------------------------------------------------------
;; routines to be called from program
;; ------------------------------------------------------------

;; save and load is not supported at this time
SAVE:
                RTS
LOAD:
                RTS

MONCOUT:        PHA
                LDY YOUT
                CMP #$07        ; ignore cursor flash character
                BEQ EXITMONCOUT
                CMP #$0A        ; ignore the LF characters
                BEQ EXITMONCOUT
                CMP #$0D        ; CR? go to new line
                BEQ :+

                JSR CONVASCII   ; convert the ascii here
                STA DSP,Y
                INY
                CPY #$1E        ; when running a program that will print to the screen
                BMI :++         ; check if line reaches max length and move to a new
                  : JSR CLEARLINE ; line if necessary
                  LDY #$00
                : STY YOUT
EXITMONCOUT:
                PLA
                RTS

; call this during NMI (after PPU control is finished) to check the keyboard
; buffer and read the keyboard if buffer is empty
; some key presses may already be buffered, we want to return those
; first before looking for new keys from the host
KBDREADY:       STY YSAV
                LDY kbreadp
LBUF:           CPY #$04
                BEQ REFBUFFER         ; no release key in buffer
                LDA kbread,Y
                CMP #$00
                BNE BUFREADY
                INY
                JMP LBUF
BUFREADY:       STY kbreadp
                LDY YSAV
                RTS 

REFBUFFER:      LDY YSAV                ; restore Y
                LDA #$00                ; reset pointer index
                STA kbreadp
                LDA kbdetect
                CMP #$01                ; is it family keyboard?
                BEQ REFFAMIKBD
                CMP #$02                ; is it keyboard mouse host?
                BEQ REFKBMH
                JSR KBINIT                ; neither? init the keyboard again ?
                JMP EXITREADY
REFFAMIKBD:     JSR CLEARKBUF           ; family basic keyboard might (usually) load less than
                JSR READFAMIKBD         ; four keycodes, so wipe the buffer contents first
                JMP EXITREADY
REFKBMH:        JSR READKBMH
EXITREADY:      RTS

; clear out the keyboard buffer
CLEARKBUF:      TYA
                PHA
                LDY #$03
                LDA #$00
                : STA kbread,Y
                DEY
                BPL :-
                PLA
                TAY
                RTS

; call this during output to prevent echo to screen
SILENTRDKEY:    STY YSAV
                LDY kbreadp             ; load the pointer
                LDA kbread,Y           ; load the character in buffer
                CMP #$00
                BEQ :+
                INY                     ; advance the pointer
                STY kbreadp             ; store the pointer
                CPY #$04                ; end of buffer, wait for more input
                BEQ :+        
                CMP #$08
                BNE :+
                LDA #'_'                ; msbasic expects _ when backspace pressed
                : LDY YSAV
                RTS

; call this in your main program loop to read the keyboard buffer
; use the CMP flags to decide what to do
MONRDKEY:
READKBD:        STY YSAV
                LDY kbreadp             ; load the pointer
                : LDA kbread,Y           ; load the character in buffer
                CMP #$00
                BEQ NOKEYPRESS
                INY                     ; advance the pointer
                STY kbreadp             ; store the pointer
                CPY #$04                ; end of buffer, wait for more input
                BEQ NOKEYPRESS          
                CMP #$80                ; jump over release keys
                BCS :-
                CMP #$08
                BNE :+
                LDA #'_'                ; msbasic expects _ when backspace pressed
                : JSR CHECKMODKEYS       ; swap to shift key if shift pressed
                LDY YSAV
                CMP #$0D                ; skip outputting the return character
                BEQ :+                  ; to screen, basic will send that
                JSR OUTDO
                : RTS
NOKEYPRESS:                             ; only vbwait if there are no keys in the
                LDY YSAV                ; buffer left to process
                JSR VBWAIT
                RTS



;; ------------------------------------------------------------
;; internal routines
;; ------------------------------------------------------------

;; discover which keyboard mode to use and prepare it
;; detect keyboard present
;; first, try family basic keyboard
KBINIT:         LDA #$05          ; enable family basic keyboard
                STA JOYPAD1
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                LDA #$04
                STA JOYPAD1
                JSR FAMIDELAY
                LDA JOYPAD2
                AND #$1E
                CMP #$1E          ; check all 1s for row 0 column 0
                BNE KBMHOST
                LDA #$01
                STA JOYPAD1       ; disable family basic keyboard
                JSR FAMIDELAY          
                LDA JOYPAD2
                AND #$1E
                CMP #$00          ; check all 0s for row 0 column 0
                BNE KBMHOST
                LDA #$01
                JMP ENDINIT
                ;; next, try keyboard mouse host
KBMHOST:        JSR READKBMH
                LDA mseread
                AND #$16          ; sets 0 if not 0x16
                BEQ WAIT4INIT
                LDA #$02
                JMP ENDINIT
WAIT4INIT:      JSR VBWAIT        ; keep waiting until keyboard found
                JMP KBINIT
ENDINIT:        STA kbdetect
                RTS

; check if any modkeys set alter the keycodes
CHECKMODKEYS:   PHA
                AND #$7F
                STA fkbtemp
                LDA KBMODKEY
                AND #$22                ; is any shift key held?
                BEQ EXITMODCHECK
                LDA kbdetect            ; load address of shiftmap depending on
                CMP #$01                ; if we are in family basic keyboard
                BNE :+
                LDA #<FBKBSHIFTMAP
                STA $FE
                LDA #>FBKBSHIFTMAP
                STA $FF
                JMP LOOPMODMAP
                : LDA #<KBMSHIFTMAP     ; or in keyboard mouse host modes
                STA $FE
                LDA #>KBMSHIFTMAP
                STA $FF
LOOPMODMAP:     LDY #$00
                LDA fkbtemp              ; check if an ascii code
                CMP #$40
                BCC :+
                CMP #$5B                 ; if so, add 0x20 to replace
                BCS :+
                CLC
                ADC #$20
                JMP RESTOREACTION       
                : LDA ($FE),Y
                CMP #$00                
                BEQ EXITMODCHECK        ; reached the end of the shiftmap table
                CMP fkbtemp
                BEQ REPLACEKEY          ; key is in shiftmap table, replace it
                INY
                JMP :-
REPLACEKEY:     TYA
                CLC
                ADC #$18                ; shift to replacement table index
                TAY
                LDA ($FE),Y
RESTOREACTION   : STA fkbtemp
                PLA
                AND #$80                ; get up/down state from original code
                ORA fkbtemp             ; and apply to replaced key code
                RTS
EXITMODCHECK:   PLA
                RTS

; read input from keyboard mouse host
READKBMH:       PHP
                TYA
                PHA

                LDY #$00
                ; strobe $4016 once to signal start reading
                LDA #$00
                STA JOYPAD1
                NOP
                NOP
                NOP
                LDA #$01
                STA JOYPAD1
                NOP
                NOP
                NOP
                LSR A
                STA JOYPAD1
                ; loop this four times total for 4 bytes
LOOPFOUR:       LDA #$01
                STA mseinput
JOY2LOOP:       LDA JOYPAD2
                LSR A ; D0
                LSR A ; D1
                LSR A ; D2
                LSR A ; D3
                ROL kbinput
                LSR A ; D4
                ROL mseinput
                BCC JOY2LOOP

                STY YSAV              ; we can make use of YSAV here, it won't conflict
                LDA kbinput
                AND #$80
                STA fkbtemp           ; store up/down status
                CMP #$00 
                LDA kbinput
                BEQ :+
                AND #$7F              ; get usb code
                CMP #$78
                BCC :+
                JSR SETMODKEYS
                : TAY
                LDA KBMHOSTMAP,Y      ; load ascii code
                CMP #$00
                BEQ STOREASCII        ; if the code is invalid, don't restore
                CMP #';'              ; we are only looking to mod ; right now, create subroutine for more handling
                BNE :+
                PHA
                LDA KBMODKEY
                AND #$22              ; is any shift key held?
                TAY
                PLA
                CPY #$00
                BEQ :+
                LDA #':'              ; could do math, just replace for now
                : ORA fkbtemp          ; restore status
STOREASCII:     LDY YSAV
                STA kbread,Y
                LDA mseinput
                STA mseread,Y
                INY
                CPY #$04
                BNE LOOPFOUR

                PLA
                TAY
                PLP
                RTS

;; set the status of the mod keys; shift, alt, ctrl
;; for keyboard mouse host mode
SETMODKEYS:     PHA                     ; save the keycode
                SEC
                SBC #$77                ; get an offset 1-8
                TAY
                SEC
                LDA #$00
                : ROL                    ; roll a bit into the right position
                DEY
                BNE :-
                PHA
                LDA fkbtemp
                AND #$80
                BNE SUBMODKEY
                PLA
                ORA KBMODKEY            ; if key down, set 1 in key position
                JMP RETMODKEY
SUBMODKEY:      PLA
                EOR KBMODKEY            ; if key up, set 0 in key position
RETMODKEY:      STA KBMODKEY
                PLA
                RTS


;; read input from the family basic keyboard
READFAMIKBD:    PHP
                TYA
                PHA
                TXA
                PHA

                LDY #$00
                LDA #$05
                STA JOYPAD1              ; strobe family keyboard for report
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                LDA #$04
                STA mseinput             ; we can use mseinput here safely, too
readkeyboard:   STA JOYPAD1               
                JSR FAMIDELAY
                NOP
                NOP
                LDA JOYPAD2
                LSR
                CLC
                AND #$0F
                STA fkbtemp             ; store in fkbtemp
                CMP FAMIKEY,Y           ; if same as history, ignore
                BEQ nextkeyboard  
                LDA FAMIKEY,Y           ; we want to check if a key has been released
                ORA fkbtemp             ; get all 1 positions
                EOR fkbtemp             ; get the released positions
                LDX #$04
parsekeyboard:  DEX
                LSR
                BCC finishparse         ; if 0, skip
                PHA                     ; backup kb data
                TYA                     ; 
                PHA                     ; and Y
                ASL                     ; multiply by 4 to get word pos
                ASL
                STA kbinput             ; we can use kbinput here, it won't conflict with kbmhost
                CLC
                TXA
                ADC kbinput             ; location of target character
                TAY
                LDA FBKBMAP,Y
                CMP #$00
                BEQ restorekbdata
                CMP #$78
                BCC :+
                JSR SETMODKEYS
                :LDY kbreadp         
                CPY #$04                ; do we have four new keys in buffer already?
                BEQ restorekbdata       ; we might lose key presses this way, rarely
                STA kbread,Y
                INY
                STY kbreadp
restorekbdata:  PLA                     ; restore kb data
                TAY
                PLA
finishparse:    CPX #$00
                BNE parsekeyboard
nextkeyboard:   LDA fkbtemp 
                STA FAMIKEY,Y           ; store new keyboard values
                LDA mseinput
                EOR #$02                ; flipflop the 2nd bit to switch column/row
                STA mseinput
                INY
                CPY #$12
                BCC readkeyboard
exitkeyboard:   JSR FAMIMODKEYS
                LDA #$00
                STA kbreadp

                PLA
                TAX
                PLA
                TAY
                PLP
                RTS

;; set the status of the mod keys; shift, ctrl
;; for family keyboard
FAMIMODKEYS:    LDA #$20                ; load a bit for rshift
                STA fkbtemp
                LDA FAMIKEY+1
                AND #$02                ; rshift in the second bit
                JSR FAMISETMODKEY
                LDA #$01                ; load a bit for lctrl
                STA fkbtemp
                LDA FAMIKEY+14
                AND #$08                ; lctrl in the fourth bit
                JSR FAMISETMODKEY
                LDA #$02                ; load a bit for rshift
                STA fkbtemp
                LDA FAMIKEY+15
                AND #$01                ; lshift in the first bit
                JSR FAMISETMODKEY
                RTS
FAMISETMODKEY:  BEQ :+
                LDA fkbtemp
                EOR #$FF
                AND KBMODKEY
                JMP EXITFSMD
                : LDA fkbtemp
                ORA KBMODKEY
EXITFSMD:       STA KBMODKEY
                RTS

;; family basic keyboard need ~50 cycles of delay
FAMIDELAY:      LDX #$0A                ; this shouldn't be getting called any time
                : DEX                   ; X is hot
                BNE :-
                RTS

; ascii values map directly to charmap
; return blank for anything below A0
CONVASCII:      CMP #$08                ; check for backspace
                BNE :+
                LDA #$3F                ; print an underscore if backspace
                : CMP #$20
                BCC :+
                .ifdef CONFIG_1K_CHR
                CMP #$60                ; return blank for anything above 60
                BCS :+                  ; for single chip cartridge only
                .endif
                SEC
                SBC #$20                ; subtract A0 to get the character
                JMP RETCHR
                : LDA #$00
RETCHR:         RTS

;; PPUSTATUS bit 7 is unreliable for vblank detection
;; use a flag in RAM instead, so the NMI handler knows it's safe to run
VBWAIT:         SEC              ; set NMI ready flag
                ROR READY
                : BIT READY      ; and wait until the NMI handler clears it
                BMI :-
                RTS

NMI:            BIT READY        ; abort if not ready yet
                BPL IRQ
          
                PHA              ; push contents of flags, and registers onto stack
                TXA
                PHA
                TYA
                PHA

;; TakuikaNinja - allows for writes to the $0200 page to be reflected in OAM.
                LDA #$00 	; Set SPR-RAM address to 0
                STA OAMADDR
                LDA #$02        ; Set OAMDMA address to $0200
                STA OAMDMA
                LDA #%00011110  ; Enable sprites and background
                STA PPUMASK

;; transfer DSP contents to the PPU nametable
                BIT PPUSTATUS
                LDA VSCROLL+1
                STA PPUADDR
                LDA VSCROLL
                STA PPUADDR
                
                LDY #$00
                : LDA DSP,Y
                INY
                STA PPUDATA
                CPY #$1E
                BMI :-

;; clear an extra line after the input display so the vertical mirroring isn't apparent on the bottom row
                LDA VSCROLL
                PHA
                LDA VSCROLL+1
                PHA
                JSR INCVSCROLL

                BIT PPUSTATUS
                LDA VSCROLL+1
                STA PPUADDR
                LDA VSCROLL
                STA PPUADDR

                LDA #$00
                TAX
                LDY #$1E
                : STA PPUDATA
                DEY
                BNE :-

                PLA
                STA VSCROLL+1
                PLA
                STA VSCROLL

                BIT PPUSTATUS
                STX PPUSCROLL   ; set scroll
                LDA VSCROLLY
                STA PPUSCROLL
                LDA #%10000000  ; select nametable and keep NMI enabled
                STA PPUCTRL

                ; with all the PPU handling done, check the keyboard buffer
                JSR KBDREADY

                ; restore contents of flags and registers from stack
                PLA
                TAY
                PLA
                TAX
                PLA

                ASL READY       ; clear NMI ready flag
IRQ:
                RTI

CLEARLINE:      PHA
                JSR VBWAIT      ; wait before clearing lines
                JSR INCVSCROLL  ; increase the vscroll

;; keep the vertical scroll fixed until we reach row $1d, to simulate the original Apple 1 terminal
;; this also ensures that the text display is mostly within the "action safe" area
;; see: https://www.nesdev.org/wiki/Overscan
                INC ROW
                LDA #$1B        ; reduce by 1 because we start on second row
                CMP ROW
                BCS :+
                STA ROW
                LDA #$08
                ADC VSCROLLY
                CMP #$F0
                BCC SKIPOVER
                LDA #$00
SKIPOVER:       STA VSCROLLY
                ; clear DSP so it will write a blank line
                : LDA #$00
                LDY #$20
                : STA DSP,Y
                DEY
                BPL :-
                TAY             ; return "cursor" to the start
                STY YOUT

                JSR VBWAIT      ; wait for v-blank to update changes
                PLA
                RTS

INCVSCROLL:     LDA VSCROLL+1
                CMP #$23
                BNE INCVSCROLLL
                LDA VSCROLL
                CMP #$A1
                BNE INCVSCROLLL
                LDA #$20
                STA VSCROLL+1
                LDA #$01
                STA VSCROLL
                RTS
INCVSCROLLL:    CLC
                LDA #$20
                ADC VSCROLL
                STA VSCROLL
                BCC :+
INCVSCROLLH:    INC VSCROLL+1
                : RTS


.segment "RODATA"

PALETTES:
  ; Background Palette
  .byte $0F, $00, $00, $00
  .byte $0F, $12, $00, $00
  .byte $0F, $00, $00, $00
  .byte $0F, $00, $00, $00

  ; Sprite Palette
  .byte $0F, $20, $17, $29
  .byte $0F, $07, $00, $00
  .byte $0F, $1A, $00, $00
  .byte $0F, $34, $00, $00


FBKBMAP: ; this maps the family keyboard to ascii values; ignore $00
  .byte $5D, $5B, $0D, $00, $00, $00, $00, $00 ; row 0
  .byte $3B, $3A, $40, $00, $5E, $2D, $2F, $5F ; row 1
  .byte $4B, $4C, $4F, $00, $30, $50, $2C, $2E ; row 2
  .byte $4A, $55, $49, $00, $38, $39, $4E, $4D ; row 3
  .byte $48, $47, $59, $00, $36, $37, $56, $42 ; row 4
  .byte $44, $52, $54, $00, $34, $35, $43, $46 ; row 5
  .byte $41, $53, $57, $00, $33, $45, $5A, $58 ; row 6
  .byte $00, $51, $1B, $00, $32, $31, $00, $00 ; row 7
  .byte $00, $00, $00, $00, $00, $08, $20, $00 ; row 8
; these are the lookup tables for shift modifiers
FBKBSHIFTMAP:  
  ; the first block is normal keys
  .byte $30, $31, $32, $33, $34, $35, $36, $37
  .byte $38, $39, $2D, $5B, $5D, $5C, $3B, $3A
  .byte $40, $2C, $2E, $2F, $00, $00, $00, $00
  ; the second is their counterpart + $15
  .byte $00, $21, $22, $23, $24, $25, $26, $27
  .byte $28, $29, $3D, $7B, $7D, $5F, $2B, $2A
  .byte $60, $3C, $3E, $3F, $00, $00, $00, $00

KBMHOSTMAP: ; this maps the input values to the ascii values; ignore $00
            ; wozmon expects key releases to be returned +0x80
  .byte $00, $00, $00, $00, $41, $42, $43, $44 ; 00 - 07
  .byte $45, $46, $47, $48, $49, $4A, $4B, $4C ; 08 - 0F
  .byte $4D, $4E, $4F, $50, $51, $52, $53, $54 ; 10 - 17
  .byte $55, $56, $57, $58, $59, $5A, $31, $32 ; 18 - 1F
  .byte $33, $34, $35, $36, $37, $38, $39, $30 ; 20 - 27
  .byte $0D, $1B, $08, $00, $20, $2D, $3D, $5B ; 28 - 2F
  .byte $5D, $5C, $00, $3B, $27, $00, $2C, $2E ; 30 - 37
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 38 - 3F
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 40 - 47
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 48 - 4F
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 50 - 57
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 58 - 5F
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 60 - 67
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 68 - 6F
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 70 - 77
  .byte $00, $00, $00, $00, $00, $00, $00, $00 ; 78 - 7F
KBMSHIFTMAP:
  ; the first block is normal keys
  .byte $30, $31, $32, $33, $34, $35, $36, $37
  .byte $38, $39, $2D, $5B, $5D, $5C, $3B, $27
  .byte $60, $2C, $2E, $2F, $00, $00, $00, $00
  ; the second is their counterpart + $15
  .byte $29, $21, $40, $23, $24, $25, $5E, $26
  .byte $2A, $28, $5F, $7B, $7D, $7C, $3A, $22
  .byte $7E, $3C, $3E, $3F, $00, $00, $00, $00

.ifdef CONFIG_1K_CHR
TILEDATA: .incbin "./chars1k.chr"
.else
TILEDATA: .incbin "./chars4k.chr"
.endif
