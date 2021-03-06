; Harvesting file
	XDEF HARVESTATION
	XREF disp_count, space_check, row_check		;Stepper Motor References
	XREF disp, harvested, display_string									;Array References
	XREF field, match, field_finished, CROP_FINISHED, water									;Field References
	XREF SPEAKER, KEYPADloop								;File References
	XREF second_pass, second, twentyonesec_pass, twentyone_sec							;Timing References
	XREF pattern, PTS
MyCode				section
HARVESTATION:
			pshd
			pshy
			pshx
			sei
			cli
;*******************************************************************************************************************
;************************************STEPPER MOTOR: HARVEST THE FIELD***********************************************  
            movb	#7, disp_count  ;set disp_count for the first 7 '$'
            movb    #2, space_check ;set space2 for the spaces between the '$'
            movb    #0, row_check   ;clear row_check to start checking positioning of the plowing process                             
            ldy     #disp      		;prepare to use disp to store and display '$' and ' ' accordingly
            movb    #8, field		;field = 8 means that harvesting is in progress
            jsr		SPEAKER			;SPEAKER: long_note
                        
;row_check:
; meant to check the pattern of the harvesting process 
                       
harvest7:   movb    #$80,pattern                    ; Displays the first and last 7 '.' on the field
harvest7new:
            brclr   second_pass, #$01, harvest7     ;eternal loop until 1 second has passed
            brclr   second_pass, #$01, harvest7     ;eternal loop until 1 second has passed
            clr     second_pass                     ;reset "second" flag
            movb	#'$', 1,y+                      ;display that one section of the field is harvested
            ldaa    pattern
            staa    PTS
            lsra
            lsra								;lsr(register) is one shift to the right rol(register) is one to the left also can be done with lsb(register)
            staa    pattern
            jsr		SPEAKER
            pshd                                    ;prepare to update and display field
            ldd     #disp
            jsr     display_string     
            puld
            ldaa  	disp_count                      ;disp_count = 0?
            dbeq    a, harvestspace                   ;if so display the spaces
           	staa    disp_count                      ;otherwise save decremented plow for next time
           	bra     harvest7new                     ;continue to loop until disp_count = 0

harvestspace:     
            ldaa    row_check                       
            cmpa    #2                              ;row_check = 2?
            beq     finalharvest                    ;exit out of plowing and notify user
            movb    #' ', 1,y+                      ;spaces separates the first 7 '$' on the LCD's to the second 7 '$' of each row            
            ldaa    space_check                     ;check if 2 spaces has been displayed
            dbeq    a, prepare_secondharvest        ;if space_check =  0, reset variables
            staa    space_check                     ;otherwise save decremented space variable for next time
            bra     harvestspace                    ;loop one more time to display ' '
            
prepare_secondharvest:                           
			ldaa    row_check                       ;check to see if the second14 has already been displayed
			cmpa    #1                              ;row_check = 1?
			beq     last_harvestcheck	            ;branch and incremenet row_check flag one last time
			movb	#14, disp_count                 ;set disp_count for the second 14 '#'
			movb    #2, space_check                 ;set
			movb    #1, row_check                   ;row_check = 1
			pshd
            ldd     #disp
            jsr     display_string
            puld
                      
harvest14:  movb    #$01,pattern                      ; Displays the "middle" 14 '#' on the field
harvest14new:
            brclr   second_pass, #$01, harvest7       ;eternal loop until 1 second has passed
            clr     second_pass                     ;reset "second has passed" flag
            movb	#'$', 1,y+                      ;display that one section of the field is plowed
            ldaa    pattern
            staa    PTS
            rola
            rola								;lsr(register) is one shift to the right rol(register) is one to the left also can be done with lsb(register)
            staa    pattern
            jsr		SPEAKER
            pshd                                    ;prepare to update and display field
            ldd     #disp
            jsr     display_string     
            puld
            ldaa  	disp_count
            dbeq    a, harvestspace                   ;if so display the spaces
           	staa    disp_count                      ;otherwise save decremented plow for next time
           	bra     harvest14new                         ;continue to loop until disp_count = 0

last_harvestcheck:
            movb    #2, row_check                   ;row_check = 2
            movb    #7, disp_count                  ;reset disp_count for the last 7 #'s
            lbra    harvest7
    
finalharvest:      
            ;movb	#1, LCDwaitflag	                ;tell interrupt to delay for several seconds
            movb	#1, field_finished
            movb    #0, field                       ;field = 2 = field has been planted
            clr		twentyonesec_pass				;clear out necessary times at this time
            clr		twentyone_sec					;to properly measure the growth of the crops
            clr		second							;from one stage to the next (field = 2->3->4)
            clr     disp_count
            clr     row_check 
            clr	    space_check
           ;clear out the fertilize and humidity flag  
     	    movb    #0,disp+32                         ;string terminator, acts like '\0'  
           	ldd     #disp
           	jsr     display_string
           	           	
;********************************FIELD HAS BEEN PLANTED*************************************
           	movb	#1, CROP_FINISHED
           	ldd  	#harvested
           	jsr  	display_string             
LOOP5:     	
			jsr		KEYPADloop
           	ldab	match
           	cmpb	#$F
           	beq		EXIT_HARV
           	movb	#1, CROP_FINISHED
           	bra		LOOP5
           	;lbeq	Proceed	

EXIT_HARV:
			movb	#0, field
			movb	#0, water
			pulx
			puly
			puld
			rts		;exit back out into the keypad