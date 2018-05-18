; LED Lighting file
	XDEF LED_Light
	XREF PTS, SPEAKER
	XREF field, qrtsec_pass, IRQflag, INITIALIZE

LED_Light:
			sei
			cli
			pshd
			pshx
			pshy
			
			;CHECK FLAGS AND PERFORM LED ACTIONS ACCORDINGLY
			ldaa	IRQflag				    ;prepare IRQflag for when the IRQ is called
			cmpa	#1						;if flag = 1, prepare for alternating LED displays
			beq		repeatLED0
			ldaa	field
			cmpa	#0
			beq		repeatLED1
			lbra	EXIT					;otherwise, make no changes....NO POINT IN GOING HERE BOOOIIIII
;*******************************************************************************************************************
;**********************************LED PATTERN: IRQ INTERRUPT SEQUENCE**********************************************
repeatLED0:
			movb	#%10101010, PTS
			jsr SPEAKER							;display LED pattern (part 1)
repeatLED01:
			brclr   qrtsec_pass, #$01, repeatLED01
			clr		qrtsec_pass
			movb	#%01010101, PTS						;display LED pattern (part 2)
repeatLED02:
			brclr   qrtsec_pass, #$01, repeatLED02
			clr		qrtsec_pass
			movb	#$00, PTS							;clear out the LED lights once more
			lbra	EXIT

;*******************************************************************************************************************
;****************************************LED PATTERN: WELCOME MENU**************************************************			
repeatLED1:
			movb	#$81, PTS
repeatLED11:
			brclr   qrtsec_pass, #$01, repeatLED11
			clr		qrtsec_pass
			movb	#$42, PTS
repeatLED12:
			brclr   qrtsec_pass, #$01, repeatLED12
			clr		qrtsec_pass
			movb	#$24, PTS
repeatLED13:
			brclr   qrtsec_pass, #$01, repeatLED13
			clr		qrtsec_pass
			movb	#$18, PTS	
repeatLED14:
			brclr   qrtsec_pass, #$01, repeatLED14
			clr		qrtsec_pass
			movb	#$00, PTS					
			bra		EXIT
;*******************************************************************************************************************
;*******************************LED PATTERN: EXITING OUT OF THE SUBROUTINE******************************************			
EXIT:	
			puly
			pulx
			puld
			rts				