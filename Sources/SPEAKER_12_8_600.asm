;Speaker File
    XDEF 	SPEAKER
    XREF	lostwoods, plow_song, plant_song, harvest_song, fertilize_song, irrigate_song, scaleup, scaledown, pest_song, epona
    XREF	SendsChr,PlayTone, qrtsec_song, halfsec_song, second
    XREF	LED_Light
    ;countRTI_flag tuneCount, qrtsec_pass, song_pass, 
    XREF	field, water, IRQflag


;My local variables
MY_RAM:				        SECTION

MyCode:     	SECTION
SPEAKER:
		sei
		cli
		pshd
		pshx
		pshy
;*******************************************************************************************************************
;*************************SPEAKER: PREPARING FOR DIFFERENT SONGS AND CHECKING FLAGS*********************************		
		ldaa	IRQflag
		cmpa	#1
		lbeq	pestsong
		
		ldaa	water
		cmpa	#1
		lbeq	fertilizesong		; water = 1 = stage of fertilizing
		cmpa	#2					
		lbeq	irrigatesong		; water = 2 = stage of irrigating
		
		ldaa	field
		cmpa	#0
		lbeq	welcomesong			; field = 0 = empty field
		cmpa	#6					
		lbeq	plowsong 			; field = 6 = stage of plowing
		cmpa	#7
		lbeq	plantsong			; field = 7 = stage of planting
		cmpa	#8					
		lbeq	harvestsong			; field = 8 = stage of harvesting         
;DC MOTOR path	
        
		lbra	EXIT

;*******************************************************************************************************************
;****************************SPEAKER: PLAYING THE WELCOME SONG UPON PROGRAM STARTUP*********************************
welcomesong:				
		ldx		#epona             ;debug song selection
repeat_welcomesong:
		ldab  	second					;loads up real-time interval value as index value
		ldaa 	b,x                    	;A will store the values from the array of X via indexing by B
		pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb	
			
		cmpb 	#11						;has the song reached it's 8th element?
		blt		repeat_welcomesong
		lbra	EXIT


	
		
plowsong:				
		ldx		#plow_song              ;notes in x
repeat_plowsong:
		ldab	second			;loads
		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
		pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb		
		cmpb 	#1 			;how long is the song?
		blt		repeat_plowsong
		lbra		EXIT
		
plantsong:				
		ldx		#plant_song              ;notes in x
repeat_plantsong:
		ldab 	second				;loads
		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
			pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb		 
			cmpb 	#1			;how long is the song?
		blt		repeat_plantsong
		lbra		EXIT

		
harvestsong:				
		ldx		#harvest_song              ;notes in x
repeat_harvestsong:
		ldab 	second				;loads
		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
			pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb		
			cmpb 	#1			;how long is the song?
		blt		repeat_harvestsong
		lbra		EXIT
		
		
fertilizesong:				
		ldx		#scaleup				 ;fertilize_song         ;
repeat_fertilizesong:
		ldab 	second				;loads
		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
			pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb		
			cmpb 	#8			;how long is the song?
		blt		repeat_fertilizesong
		lbra		EXIT
		
irrigatesong:
			ldx		#scaledown              ;notes in x		irrigatesong				;
repeat_irrigatesong:
		ldab 	second				;loads
		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
		pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb		
		cmpb 	#8			;how long is the song?
		blt		repeat_irrigatesong
		lbra		EXIT
		
;may do this elsewhere		
pestsong:
		ldx		#pest_song				;scaledown              ;notes in x
repeat_pestsong:
		ldab 	second				;loads
		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
		pshb
		pshx
		psha
		jsr		SendsChr
		jsr		PlayTone
		pula
		pulx
		pulb		
			cmpb 	#1			;how long is the song?
		blt		repeat_pestsong
		lbra		EXIT2
		
EXIT:
		movb	#0, second	
		puly
		pulx
		puld
		rts
		
EXIT2:
		puly
		pulx
		puld
		rts				
								
		
		












;speaker:		
;ianSpeakerLp	ldaa	song_pass
;				ldx		#plow_song ;load the song you want to play
;				ldab	a,x
;				psha
;				pshx
;				pshb
;				jsr		SendsChr
;				jsr		PlayTone
;				pulb
;				pshx
;				psha
;;				cmpa	#20		;number of notes
;				blt		ianSpeakerLp
;				clr		song_pass
;				inc		tuneCount
;				ldaa	#1
;				cmpa	tuneCount
;				bne		ianSpeakerLp
;				rts		;might want to branch to exit at bottom of file



;ldd		speakerFlag2
;			addd	#1
;			std		speakerFlag2
;			cpd		#7852
;			bne		exitSpeaker
;			inc		song_pass
;			movw	#0, speakerFlag2


;welcomesong:				
;		ldx		#lostwoods              ;notes in x
;repeat_welcome:
;		
;		ldab 	countRTI_flag				;was song_pass
;		ldaa 	b,x                    ;notes,x offset by rhythmn,a in b
;		pshb
;		pshx
;		psha
;		jsr		SendsChr
;		jsr		PlayTone
;		pula
;		pulx
;		pulb
;		;brclr   qrtsec_pass, #$01, play1
;		;clr		qrtsec_pass
;		;inc 	counter
;		;ldab	counter
;		; ldaa element
;		; psha
;		; jsr SendsChr
;		; pula
;		; inc count
;		; call PlayTone until count = number
;		; repeat up to ldaa element
;		cmpb 	#40			;how long is the song?
;		blt		repeat_welcome
;		lbra		EXIT



