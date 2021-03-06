; Initialization file
    XDEF INITIALIZE
    XREF ddr_p, Uddr, port_u_pol, port_u_cont, portU, Sddr, DDR_T, INTCR                        ;Port Initialization 
    XREF anticlockwise, clockwise, anticlock_temp, clockwise_temp, dcflag, DC_counter, DC_Speed ;Motor Initialization
    XREF qrtsec_song, halfsec_song, second, twentyone_sec, count_RTI
    XREF RTICTL, CRGINT, qrtsec_pass, halfsec_pass, second_pass, threesec_flag, twentyonesec_pass, countRTI_flag        ;Timing Initilization
    XREF field, water, match, irrigate, fertilizer, field_finished, plow_var, plant_var, harv_var  ;Field/LCD Initialization
    XREF IRQflag, CROP_FINISHED
    XREF LEDDisp_count, stageflag
    ;XDEF LCDwaitflag, tuneCount,speakerFlag2 qrt_sec, doubleLED, 
;code section
MyCode:			SECTION
INITIALIZE:
		
		movb	#$1E, ddr_p			; Initialize port p motor with 00011110
        movb  	#$F0, Uddr          ;hexpad DDR: (upper nibble/rows/vertical is output. lower nibble/columns/horizontal is input)
        movb  	#$F0, port_u_pol    ;hexpad polarity
        movb 	#$0F, port_u_cont   ;hexpad control register
        movb  	#$FF, portU         ;hexpad
        movb  	#$FF, Sddr		  	; LEDs output via ddr
        movb	#$28, DDR_T		    ; set bit 3 and 5 to make DC Motor and speaker operatable
        movb    #$C0, INTCR         ; 
               
                                    ;Field/LCD Initilization
        movb	#15, plow_var		;DC MOTOR: counter for speed of plowing
        movb	#60, plant_var      ;DC MOTOR: counter for speed of planting
        movb	#15, harv_var		;DC MOTOR: counter for speed of harvesting
		movb    #0, field			; Clear field flag (for the main)
		movb	#0, water			; Clear water flag (for the main)
		movb    #0, irrigate        ; Clear irrigate flag (for the main)
		movb	#0, fertilizer		; Clear fertilizer flag (for the main)
        movb    #0, match			; Clear keypad index (for the keypad)
        movb	#0, field_finished  ; Clear field_finished flag (for the Stepper mot)
        
                                                ;Motor Initilization                                        
        movw	#anticlockwise, anticlock_temp	;set the address of anticlockwise to the temp
        movw	#clockwise, clockwise_temp		;set the address of clockwise to the temp
        movw	#260, DC_counter
        movb	#0, dcflag
        movb	#0, DC_Speed					;variable to temp. hold associated DC motor speed
        
                                    ;Timing Initilization
        movb	#$10, RTICTL		; Set time interval to 0.000128ms
		movb	#$80, CRGINT		; Enable interrupt
        movb	#0, countRTI_flag	; flag is set after each time the interrupt occurs
        movb    #0, qrtsec_pass		; flag is set after every quarter second
        movb	#0, halfsec_pass	; flag is set after every half second
        movb    #0, second_pass		; flag is set after every second
        movb    #0, threesec_flag	; flag is set after every three seconds
        movb	#0, twentyonesec_pass	; flag is set after every twenty-one seconds
   
        movw    #00, count_RTI
        movb	#0, qrtsec_song 
        movb	#0, halfsec_song
        movb	#0, second
        movb	#0, twentyone_sec
        ;movb	#0, LCDwaitflag
        ;movb	#0, tuneCount
        ;movw	#0, speakerFlag2
        ;movw    #00, qrt_sec
        
        ;movb	#01, doubleLED
        movb	#100, LEDDisp_count
        movb	#0, stageflag
        
        movb	#0, IRQflag
        MOVB	#0, CROP_FINISHED
        rts
            				