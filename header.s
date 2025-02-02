		.segment "HEADER"
.ifdef KBD
        jmp     LE68C
        .byte   $00,$13,$56
.endif
.ifdef AIM65
        jmp     COLD_START
        jmp     RESTART
        .word   AYINT,GIVAYF
.endif
.ifdef SYM1
        jmp     PR_WRITTEN_BY
.endif
.ifdef NES
; .byte "NES", $1A        ; NES2.0 header identifier
  .byte $4E, $45, $53, $1A
  .byte $02                 ; 2x 16KB PRG code
  .byte $00                 ; 0x  8KB CHR data
.ifdef CONFIG_1K_CHR
  .byte $A9, $D8            ; mapper 218, vertical mirroring
  .byte $00, $00, $0F, $04  ; 8KB of Work RAM, 1KB of CHR RAM
.else
  .byte $01, $08            ; mapper 0, vertical mirroring
  .byte $00, $00, $0F, $06  ; 8KB of Work RAM, 4KB of CHR RAM
.endif
  .byte $00, $00, $00, $23  ; default expansion device Family Basic Keyboard

  .segment "ISR"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr NMI
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr NES_RESET
  ;; External interrupt IRQ (unused)
  .addr IRQ
.endif
