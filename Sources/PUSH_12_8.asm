	XDEF PUSH_BUTTON
	XREF field, display_string, PTP
	XREF CropCondition0, CropCondition1, CropCondition2, CropCondition3, CropCondition4, CropCondition5A
	XREF before_menu1, in_menu1, in_menu2, plowed_menuexit, in_menu3, irrigated_menuexit, denied_message, denyplant_message, clarifyplow_message
;*******************************************************************************************************************
;**************************************PUSH BUTTON: CHECKING FIELD FLAG*********************************************
PUSH_BUTTON: 
			pshd
			pshx
			pshy         		   
		    ldaa		field
		    cmpa	    #0		        ;is the field empty?
		    beq		    EMPTY_FIELD
		    cmpa	    #1              ;is the field plowed?
		    beq		    PLOWED_FIELD
		    cmpa	    #2              ;is the field planted?
		    beq		    SOWED_FIELD
		    cmpa	    #3              ;is the field growing?
		    beq		    GROWING_FIELD
		    cmpa	    #4              ;are the crops matured?
		    beq		    MATURE_FIELD
		    cmpa	    #5              ;is the field harvestable?
		    beq		    READY_FIELD	
		    lbra		EXIT_PUSH        ;otherwise exit
          		   
;*******************************************************************************************************************
;********************************PUSH BUTTON: FIELD CONDITION DISPLAYED ON LCD**************************************          		   
EMPTY_FIELD:
	      	ldd		    #CropCondition0
		    jsr		    display_string
		    ;ldd		    #CropCondition0
		    ;jsr		    display_string
		    lbra		EXIT_PUSH

PLOWED_FIELD
		ldd		#CropCondition1
		jsr		display_string
		;ldd		#CropCondition1A
		;jsr		display_string
		lbra		EXIT_PUSH	   

SOWED_FIELD
		ldd		#CropCondition2
		jsr		display_string
		;ldd		#CropCondition2A
		;jsr		display_string
		lbra		EXIT_PUSH	 
		
GROWING_FIELD
		ldd		#CropCondition3
		jsr		display_string
		;ldd		#CropCondition3A
		;jsr		display_string
		lbra		EXIT_PUSH

MATURE_FIELD
		ldd		#CropCondition4
		jsr		display_string
		;ldd		#CropCondition4A
		;jsr		display_string	
		lbra		EXIT_PUSH
		
READY_FIELD
		ldd		#CropCondition5A
		jsr		display_string
		;brclr   PTP, #%00100000, harvesting
		;ldd		#CropCondition5A
		;jsr		display_string
		lbra		EXIT_PUSH
		
		
EXIT_PUSH:
		puly
		pulx
		puld
		rts		
		movb	#1, before_menu1		;LOOP1
		movb	#1, in_menu1			;LOOP2
		movb	#1, in_menu2			;YouMayPass1
		movb	#1, plowed_menuexit		;LOOP4
		movb	#1, in_menu3			;LOOP6
		movb	#1, irrigated_menuexit	;LOOP8
		movb	#1, denied_message		;LOOP00
		movb	#1, denyplant_message	;LOOP01
 		movb	#1, clarifyplow_message	;LOOP02