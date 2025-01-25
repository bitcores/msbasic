.segment "CODE"
ISCNTC:       JSR SILENTRDKEY           ; read a key silently from keyboard
              CMP #$43                  ; is it a C?
              BNE NOTCTRLC
              LDA KBMODKEY              ; load the modifier key states
              AND #$11                  ; is at least one ctrl key being pressed?
              BEQ NOTCTRLC
              LDA #$00                  ; STOP checks for zero flag
              JMP ISCTRLC
NOTCTRLC:     RTS
ISCTRLC:
;!!! runs into "STOP"