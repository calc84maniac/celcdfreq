	#include "ti84pce.inc"

#macro LCD_TIMING(VSW, VBP, LPP, VFP, HSW, HBP, PPL, HFP, PCD, CPL)
	#if (VSW < 1) | (VSW > 64)
		.error "VSW must be between 1 and 64"
	#endif
	#if (VBP < 0) | (VBP > 255)
		.error "VBP must be between 0 and 255"
	#endif
	#if (LPP < 1) | (LPP > 1024)
		.error "LPP must be between 1 and 1024"
	#endif
	#if (VFP < 0) | (VFP > 255)
		.error "VFP must be between 0 and 255"
	#endif
	#if (HSW < 1) | (HSW > 256)
		.error "HSW must be between 1 and 256"
	#endif
	#if (HBP < 1) | (HBP > 256)
		.error "HBP must be between 1 and 256"
	#endif
	#if (PPL < 16) | (PPL > 1024) | (PPL & 15)
		.error "PPL must be a multiple of 16 between 16 and 1024"
	#endif
	#if (HFP < 1) | (HFP > 256)
		.error "HFP must be between 1 and 256"
	#endif
	#if (PCD < 1) | (PCD > 1025)
		.error "PCD must be between 1 and 1025"
	#endif
	#if (CPL < 1) | (CPL > 1024)
		.error "CPL must be between 1 and 1024"
	#endif
	.db ((PPL >> 4) - 1) << 2, HSW-1, HFP-1, HBP-1
	.dw (LPP - 1) | ((VSW - 1) << 10) \ .db VFP, VBP
	#if PCD > 1
		.dw $7800 | ((PCD - 2) & $1F), (CPL - 1) | (((PCD - 2) << 6) & $F800)
	#else
		.dw $7800, (CPL - 1) | $0400
	#endif
#endmacro

	.db tExtTok, tAsm84CeCmp

	.org userMem

	call _boot_InitializeHardware
	di

	ld a,keyModeScan
	ld (mpKeyMode),a

	ld hl,mpLcdCtrl+1
	ld a,(hl)
	or lcdIntFront >> 8
	ld (hl),a
	call waitForFrontPorch
	call waitForFrontPorch
	dec hl
	ld (hl),lcdBpp1
	ex de,hl
	ld e,lcdTiming0
	ld hl,lcdGraphXTiming
	ld bc,12
	ldir
	ld hl,mpLcdPalette
	ld (hl),$FF \ inc hl \ ld (hl),$FF \ inc hl \ ld (hl),$00 \ inc hl \ ld (hl),$00
	call fill1bpp

	ld hl,mpSpiCtrl1+2
	ld (hl),9-1 ; 9 bits of FIFO

	ld ix,10000000/60
	call setNextTiming
mainLoop:
	ld de,spiInvertCommandList
	ld bc,vRam+(320*240/8)
	call waitForFrontPorch
	ld (mpLcdBase),bc
	call waitForFrameSwap
	call spiSendCmds
	ld de,spiUninvertCommandList
	ld bc,vRam
	call waitForFrontPorch
	ld (mpLcdBase),bc
	call waitForFrameSwap
	call spiSendCmds
	ld a,(mpKeyData+14)
	and 6
	ld b,a
	ld a,15
	jr z,_
keyDebounce = $+1
	ld a,1
	dec a
	jr z,++_
	cp 15-1
	jr z,_
	ld b,0
_
	ld (keyDebounce),a
_
	push bc
	bit 1,b
	call nz,setNextTiming
	pop bc
	bit 2,b
	call nz,setPrevTiming
	ld hl,mpKeyData+12
	bit 6,(hl)
	jr z,mainLoop
_
	bit 6,(hl)
	jr nz,-_

	ld de,spiDefaultTimingCommandList
	call spiSendCmds

	ld l,spiCtrl1+2
	ld (hl),3-1 ; 3 bits of FIFO

	ld hl,mpLcdCtrl
	ld (hl),lcdBpp16
	ex de,hl
	ld e,lcdTiming0
	ld hl,lcdDefaultTiming
	ld bc,12
	ldir

	ld a,keyModeScanOnce
	ld (mpKeyMode),a

	lea hl,ix
	ld bc,60
	call __imulu
	ei
	push hl
	call _HomeUp
	call _ClrScrnFull
	call _DrawStatusBar
	pop hl	
	jp _DispHL

	; Input: IX=divisor
	; Output: IX=previous divisor, timing updated
setPrevTiming:
	ld de,0
	ld c,e
	ld b,31
setPrevTimingLoop:
	lea hl,ix-1
	call roundDownDivisor
	jr nz,setPrevTimingContinue
	sbc hl,de
	jr c,setPrevTimingContinue
	add hl,de
	ex de,hl
	ld c,b
setPrevTimingContinue:
	djnz setPrevTimingLoop
	jr updateTiming

	; Input: IX=divisor
	; Output: IX=next divisor, timing updated
setNextTiming:
	ld de,0
	ld c,e
	ld b,31
	dec de
setNextTimingLoop:
	lea hl,ix
	scf
	call roundDivisor
	jr nz,setNextTimingContinue
	sbc hl,de
	jr nc,setNextTimingContinue
	add hl,de
	ex de,hl
	ld c,b
setNextTimingContinue:
	djnz setNextTimingLoop

	; C=RTNA, HL=new divisor
updateTiming:
	ld b,c
	inc b
	dec b
	ret z
	ex de,hl
	push hl
	pop ix
	call roundDownDivisor
	ld hl,spiNewTimingCommandList
	push hl
	inc hl
	inc hl
	ld (hl),a
	srl (hl)
	sub (hl)
	inc hl
	ld (hl),a
	inc hl
	inc hl
	inc hl
	ld (hl),b
	pop de

spiSendCmds:
	ld hl,mpSpiCtrl2
	ld (hl),bmSpiTxClr | bmSpiRxClr ; Clear FIFOs
	inc hl
	ld (hl),bmSpiTxEn >> 8 ; Enable transmit
	dec hl
	ld (hl),bmSpiChipEn
	ld l,spiData
	ld a,(de)
spiSendCmdsLoop:
	inc de
	ld b,a
	xor a
spiSendParamLoop:
	ld l,spiStatus
spiSendParamWaitLoop:
	bit bSpiTxFifoNotFull,(hl)
	jr z,spiSendParamWaitLoop
	ld l,spiData+1
	ld (hl),a
	ld a,(de)
	inc de
	dec hl
	ld (hl),a
	ld a,1
	djnz spiSendParamLoop
	ld a,(de)
	or a
	jr nz,spiSendCmdsLoop

spiWaitTx:
	; Wait for transmit to finish
	ld l,spiStatus+2
_
	bit 0,(hl)
	jr nz,-_
	dec hl
	ld a,$F0
_
	tst a,(hl)
	jr nz,-_

	dec hl
_
	bit bSpiChipBusy,(hl)
	jr nz,-_

	; Disable transfer
	ld l,spiCtrl2
	ld (hl),h
	ret

waitForFrontPorch:
	ld a,8
	ld (mpLcdIcr),a
_
	ld a,(mpLcdRis)
	and 8
	jr z,-_
	ret

waitForFrameSwap:
	ld a,4
	ld (mpLcdIcr),a
_
	ld a,(mpLcdRis)
	and 4
	jr z,-_
	ret

fill1bpp:
	ld hl,vRam
	ld (hl),l
	push hl
	pop de
	inc de
	ld bc,320*240/8
	push bc
	ldir
	pop bc
	ld (hl),$FF
	dec bc
	ldir
	ret

roundDownDivisor:
	or a
	; Input: B=RTNA, HL=divisor, carry set to round up
	; Output: NZ if out of range, otherwise A=porch (2-254), HL=rounded divisor, carry reset
roundDivisor:
	push bc
	ld c,16
	mlt bc
	inc b
	dec bc
	dec bc
	res 2,c
	jr nc,_
	add hl,bc
_
	call __idivu
	ld a,l
	sub 320 & $FF
	ld a,h
	sbc a,320 >> 8
	jr nz,_	
	ld a,l
	sub (320-1) & $FF
	cp 2+1
	jr c,_
	dec a
	call __imulu
	cp a
_
	pop bc
	ret

spiNewTimingCommandList:
	.db 3,$B2,12,12
	.db 2,$C6,15
	.db 2,$B0,$10
	.db 0

spiDefaultTimingCommandList:
	.db 3,$B2,12,12
	.db 2,$C6,15
	.db 2,$B0,$11
	.db 0

spiInvertCommandList:
	.db 1,$21
	.db 0

spiUninvertCommandList:
	.db 1,$20
	.db 0

lcdDefaultTiming:
	LCD_TIMING(3, 4, 320, 2, 4, 32, 240, 11, 4, 240)

lcdGraphXTiming:
	LCD_TIMING(1, 0, 320, 179, 8, 64, 240, 88, 2, 240)
