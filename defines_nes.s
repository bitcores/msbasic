; configuration
CONFIG_2C := 1

CONFIG_SCRTCH_ORDER := 1
CONFIG_MONCOUT_DESTROYS_Y = 1
CONFIG_NO_CR := 1
; configures build for mapper 218
; https://www.nesdev.org/wiki/INES_Mapper_218
CONFIG_1K_CHR := 1

; zero page
ZP_START1 = $00
ZP_START2 = $10
ZP_START3 = $06
ZP_START4 = $5E
ZP_START5 = $E0

;extra ZP variables
USR             := GORESTART
KBMODKEY        := $FD      ; need to set this here for nes_iscntc

; constants
STACK_TOP		:= $FD
SPACE_FOR_GOSUB := $44
WIDTH			:= 20
WIDTH2			:= 10
CKEY            := $43


; memory layout
RAMSTART2	    := $6000

; output buffer and family keyboard input monitor
DSP             := $0300
FAMIKEY         := $0330

; NES control registers
PPUCTRL         := $2000
PPUMASK         := $2001
PPUSTATUS       := $2002
OAMADDR         := $2003
OAMDATA         := $2004
PPUSCROLL       := $2005
PPUADDR         := $2006
PPUDATA         := $2007
OAMDMA          := $4014
JOYPAD1         := $4016
JOYPAD2         := $4017