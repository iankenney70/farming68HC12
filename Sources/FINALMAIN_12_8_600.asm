;**************************************************************
;* This stationery serves as the framework for a              *
;* user application. For a more comprehensive program that    *
;* demonstrates the more advanced functionality of this       *
;* processor, please see the demonstration applications       *
;* located in the examples subdirectory of the                *
;* Freescale CodeWarrior for the HC12 Program directory       *
;**************************************************************
;field index
;0 = empty
;1 = plowed	   					#
;2 = sowed with seed  				 .
;3 = growing						*
;4 = mature but not harvestable	       @
;5 = harvestable
;6 = plowing
;7 = planting
;8 = harvesting

;water index
;0 = no dc motor
;1 = fertilize
;2 = irrigate

;fertilizer or irrigate = 0 default
;fertilizer or irrigate = 1 sped up
; Include derivative-specific definitions
            INCLUDE 'derivative.inc'

; export symbols
            XDEF Entry, _Startup, RTI_ISR, IRQ_PEST, begin 
            ; we use export 'Entry' as symbol. This allows us to
            ; reference 'Entry' either in the linker .prm file
            ; or from C/C++ later on
            XREF __SEG_END_SSTACK, RTI_ENA, harvss      ; symbol defined by the linker for the end of the stack
            
            ;ClearString References
            XREF CLEARING
            XDEF disp
            ;LCD References
	        XREF init_LCD, display_string 
            ;Keypad References
            XREF KEYPAD
            XDEF match, portU, PTP, field, CropCondition5A
            ;Speaker references
            XREF SPEAKER, PlayTone, SendsChr
            XDEF lostwoods, plow_song, plant_song, harvest_song, fertilize_song, irrigate_song, scaledown, scaleup, pest_song, epona	;Song References
            XDEF qrtsec_song, halfsec_song, second			;Timing References
            XDEF field, water, irrigate, fertilizer			;Variable References
            ;song_pass 
            ;Initilization References
            XREF INITIALIZE
            XDEF ddr_p, Uddr, port_u_pol, port_u_cont, portU, Sddr, DDR_T, INTCR                            ;Port Initialization 
            XDEF anticlockwise, clockwise, anticlock_temp, clockwise_temp,dcflag, DC_counter, DC_Speed      ;Motor Initialization
            XDEF qrtsec_song, halfsec_song, count_RTI, second, twentyone_sec	  							;Counter Initialization
            XDEF RTICTL, CRGINT, qrtsec_pass, halfsec_pass, second_pass, threesec_flag, twentyonesec_pass, countRTI_flag, stageflag   ;Timing Flag Initilization
            XDEF field, irrigate, fertilizer, match, field_finished, plow_var, plant_var, harv_var			;Field/LCD Initialization
            XDEF IRQflag, LEDDisp_count
            XDEF CROP_FINISHED
            ;XDEF LCDwaitflag, song_pass, qrt_sec
            ;LED References
            XREF LED_Light
            XDEF PTS, qrtsec_pass, field      
            ;Potentiometer References
            XREF pot_value, read_pot
            ;Harvesting References
            XREF HARVESTATION
            XDEF disp_count, second_pass, second, twentyonesec_pass, twentyone_sec, space_check, row_check
            XDEF CropCondition4, harvested
            XDEF field, match, field_finished, CROP_FINISHED
            XDEF pattern
            ;Speaker Variable
            ;XDEF tuneCount, speakerFlag2, doubleLED
 
;Variable/data section
;Variables throughout the main
MY_RAM:				        SECTION
disp:	        ds.b		33		;LCD DISPLAY TO PLOW THE FIELD
field:        	ds.b    	1		;FLAG TO CHECK THE FIELD SITUATION
water			ds.b		1		;flag that tells dc motor to irrigate or fertilize
irrigate:     	ds.b    	1     	;FLAG TO CHECK THE IRRIGATION SITUATION
fertilizer:		ds.b		1		;FLAG TO CHECK THE FERTILIZING SITUATION
CROP_FINISHED:	ds.b		1		;FLAG TO CHECK THE WHOLE PROGRAM IS FINISHED

;LCD flags
space_check:  	ds.b    	1       ;CHECKS FOR THE SPACING OF FIELD
row_check:    	ds.b    	1       ;NICHE FLAG CHECK FOR FIELD
disp_count:	  	ds.b    	1       ;Accounts for # of '#' has been displayed

;Keypad flags/value - Inside the KEYPAD.asm file
match:      	ds.b    	1       ;KEYPAD index values

;Potentiometer flags
hDig:       	ds.b		1
tDig:       	ds.b    	1
oDig:       	ds.b		1

;Speaker variable
;tuneCount:		ds.b		1					   
;speakerFlag2:	ds.w		1

;Interrupt flags
countRTI_flag:	ds.b		1 
qrtsec_pass:    ds.b   		1       ; flag to notify 0.25 second has passed (LED FILE)
halfsec_pass:	ds.b		1       ; flag to notify 0.5 second has passed
second_pass:    ds.b    	1       ; flag to notify 1 second has passed
threesec_flag:  ds.b    	1       ; flag to notify 3 seconds has passed
twentyonesec_pass:	ds.b	1		; flag to notify 21/42 seconds has passed

count_RTI:	    ds.w		1       ; counts the amount of times the interrupt came in
qrtsec_song:    ds.b    	1       ; variable to count # of times 1/4th of a second has passed
halfsec_song:	ds.b		1		; variable to count # of times 1/2nd of a second has passed
second:		    ds.b		1       ; checks for number of seconds (3 in particular)
twentyone_sec:	ds.b		1		; checks for number of times 21 seconds has passed (twice in particular)
stageflag:		ds.b		1		; checks for the growth stages

debug_LED:      ds.b    	1
doubleLED:		ds.b		1		; meant to display LED lights during plowing/planting in interrupt
LEDDisp_count:	ds.b		1		; meant to track LED lights displaying time interval in interrupt
;song_pass, qrt_sec:		ds.b		1
;LCDwaitflag:	ds.b		1       ; after the plowing field section + meant to wait for 3 seconds afterwards


;Stepper Motor flags
field_finished:	ds.b		1	; checks if the field has been plowed/planted/harvested
plow_var:		ds.b		1	; counter for how long to plow
plant_var:		ds.b		1   ; counter for how long to plant
harv_var:		ds.b		1	; counter for how long to harvest
anticlock_temp:	ds.w		1	; temp variable to store current position of the stepper motor array for counterclockwise rotation
clockwise_temp:	ds.w		1	; temp variable to store current position of the stepper motor array for clockwise rotation

;DC Motor variables
T_on            ds.b  		1
T_off           ds.b  		1
dcflag          ds.b		1
DC_counter		ds.w		1
DC_Speed		ds.b		1

;IRQ flags
IRQflag:		ds.b		1
;LED pattern
pattern: 		ds.b 		1	

;Initilizing port locations  CONSTANTS
MY_ROM:			        SECTION
Port_T:	        EQU		$240
DDR_T:		    EQU		$242
portS:		    equ  	$248
Sddr:		    equ     $24a
port_p:	        equ	    $258
ddr_p:	        equ	    $25A
portU:          equ     $268
Uddr:           equ     $26a
port_u_pol:     equ     $26d
port_u_cont:    equ     $26c
	
;stepper motor
clockwise:  	dc.b	$0A,$12,$14,$0C,$0
anticlockwise:  dc.b 	$0C,$14,$12,$0A,$0

;LCD display strings
welcome:    	dc.b	'Welcome! Press 1to continue     ', 0
menu1:      	dc.b	'2) Farming Cycle3) Fertilizing  ', 0
menu2:      	dc.b    '4) Plow Field   5) Plant seed   ', 0
menu3:      	dc.b    '8) Fertilize    9) Irrigate     ', 0
LCD_PLOWED: 	dc.b    'Field Plowed!!! Press 6 to cont.', 0
LCD_PLANTED:	dc.b    'Field Planted!!!Press 7 to cont.', 0
DENY_MSG:   	dc.b    'Access Denied   Press 0 to cont.', 0
DENY_PLANT: 	dc.b    'Field not plowedPress 0 to cont.', 0
PLOW_AGAIN: 	dc.b    'Plow again???   Press 1=Yes 2=No', 0
fertInProgress 	dc.b   	'Press A to cease fertilizing    ', 0
irrInProgress  	dc.b   	'Press B to cease irrigation     ', 0
;CropCondition0A 	dc.b	'The field is empty and unplowed ', 0
CropCondition0 	dc.b	'_ _ _ _  _ _ _ __ _ _ _  _ _ _ _', 0
;CropCondition1A	dc.b	'The field is empty and plowed ', 0
CropCondition1	dc.b	'#######  ##############  #######', 0
;CropCondition2A	dc.b	'The Crops are merely seedlings ', 0
CropCondition2	dc.b	'.......  ..............  .......', 0
;CropCondition3A	dc.b	'The Crops are growing               ', 0
CropCondition3 	dc.b	'*******  **************  *******', 0
;CropCondition4A	dc.b	'Crops are mature, but not ready', 0
CropCondition4 	dc.b	'@@@@@@@  @@@@@@@@@@@@@@  @@@@@@@', 0
CropCondition5A	dc.b	'Crops mature, push to harvest   ', 0
harvested	  	dc.b	'Harvest done press f to restart ', 0
pests		  	dc.b	'PESTS DETECTED! SPRAY PESTICIDES', 0
Pest_pot		dc.b	'Increase potimeter to eradicate ', 0												;Pest_pot		dc.b	'Pesticide Saturation is:    %   ', 0  
pestkill		dc.b	'Pests wiped out reset potimeter ', 0
infertile		dc.b	'    Fertilizer        Raised    ', 0											 ;'Soil infertile  FertilizerRaised', 0
fertile		  	dc.b	'Soil already 	   fertile enough', 0
dry				dc.b	'    water           seed raised ', 0										  ;'Low Humidity water speed raised ', 0
humid			dc.b 	'Humidity already sufficient	 ', 0
tutorial		dc.b	'Press E to return to last screen', 0

;Songs		   127 is lowest pitch		   6 is absolute highest with glitches before then at 12
;lostwoods       dc.b    44,255,36,255,32,255,44,255,36,255,32,255,44,255,36,255,32,255,22,255,26,255,32,255,28,255,32,255,40,255,47,255,53,255,47,255,40,255,47,255
;plow_song		dc.b	40, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 91 ,93 ,91, 93, 91, 93, 91 ,93
epona			dc.b	22,26,29,22,26,29,22,26,29,26,29		;23,26,29,29,31
fertilize_song	dc.b    30										;10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70
irrigate_song	dc.b    50										;70, 65, 60, 55, 50, 45, 40 ,35, 30, 25, 20, 15, 10
lostwoods       dc.b    44,36,32,44,36,32,44,36,32,22,26,32,28,32,40,47,53,47,40,47    
plow_song		dc.b	101
plant_song		dc.b    120
harvest_song	dc.b    80	 
scaleup			dc.b    59, 53, 47, 44, 40, 35, 31, 30 		;this is a scale up
scaledown		dc.b	30, 31, 35, 40, 44, 47, 53, 59 			;this is a scale down d e f g a b c					
pest_song		dc.b	19
;code section
MyCode:             SECTION
Entry:
_Startup:
;*******************************************************************************************************************
;************************************INITIALIZATION VIA SUBROUTINE**************************************************
	    	lds 	#__SEG_END_SSTACK
	    	jsr    	INITIALIZE      	;SUBROUTINE: Initializing various variables and port values							
  			jsr		init_LCD			;Initialize and prepare the LCD display screen

;*******************************************************************************************************************
;************************************1) MENU SELECTION #0: WELCOME MENU*********************************************            
begin:
           	ldd  	#welcome
           	jsr  	display_string
           	movb    #0, field
           	jsr		LED_Light				;LED_LIGHT SUBROUTINE
           	jsr 	SPEAKER				;SPEAKER: welcome song     	           	          	           	
           	
;*******************************CHECKING: PRESSED "1"*********************************
LOOP1:      
			;lbra	harvss
			nop			
			;jsr		LED_Light
		    jsr		KEYPAD
           	ldab	match
           	cmpb	#1                  ;user is brought to menu selection #1              
           	bne		LOOP1
;*******************************************************************************************************************
;*****************************2) MENU SELECTION #1: FARMING AND FERTILIZING*****************************************            	
Proceed:	  
           	ldd  	#menu1
           	jsr  	display_string
           	
;*****************************CHECKING: PRESSED "2" OR "3"***************************
LOOP2:     	
		    jsr		KEYPAD
		    ldab	match
           	cmpb	#2
           	lbeq	FarmingCycle        ;check for field condition first before Farming Cycle
           	cmpb	#3
           	lbeq	DCMOTOR
           	BNE		LOOP2
           	
;**********************CHECKING: FIELD CONDITION - EMPTY/PLOWED***************         
FarmingCycle:
           	ldaa	field		    	;check for field-flag conditions
           	cmpa	#0			    	;field is empty = 0 = menu selection #2
           	beq		YouMayPass1
           	cmpa	#1			    	;field has been plowed = 1 = menu selection #2
           	beq		YouMayPass1
           	lbra	Denied		    	;cannot check field at the moment = deny user

;*******************************************************************************************************************
;****************************3) MENU SELECTION #2: PLOW FIELD AND PLANT SEED****************************************            	 
YouMayPass1:                                 
		    ldd  	#menu2
		    jsr  	display_string
		    jsr     KEYPAD
           	ldab	match
           	cmpb	#4                  ;user chooses to plow the field
           	lbeq	PlowField
           	cmpb	#5                  ;user chooses to plant the field
           	lbeq	PlantSeed
           	BNE		YouMayPass1
           	
;************************CHECKING: FIELD CONDITION - PLOWED OR NOT**************************          
PlowField:
		    ldaa	field
           	cmpa	#0                         
           	beq		YouMayPass3
           	lbra	CLARIFY_PLOW		;clarify user if field can be plowed again (field = 1 currently)
           	
;***************************STEPPER MOTOR: PLOW THE FIELD***********************************           			
YouMayPass3:
            sei
            movb    #6, field			;field = 6 means that plowing is in progress
            cli
            jsr    	CLEARING        	;SUBROUTINE: clears out disp
            movb	#7, disp_count  	;set disp_count for the first 7 '#'
            movb    #2, space_check 	;set space2 for the spaces between the '#'
            movb    #0, row_check   	;clear row_check to start checking positioning of the plowing process                             
            ldy     #disp      			;prepare to use disp to store and display '#' and ' ' accordingly
            							;SPEAKER: plow_song

;To do this: add a 
;ldaa qrt_sec
;Cmpa #6
;Beq	qtsecw/plow
                        
;row_check:
; meant to check the pattern of the plowing process 
                       
plow7:      movb    #$80,pattern                ;initialize the LED pattern for plowing                                    
            
            ; Displays the first and last 7 '#' on the field
plow7new:   brclr   halfsec_pass, #$01, plow7	;eternal loop until 0.5 second has passed		 
            clr    	halfsec_pass                 ;reset "half_sec" flag
            movb	#'#', 1,y+                  ;display that one section of the field is plowed
            ldaa    pattern
            staa    PTS
            lsra								;lsr(register) is one shift to the right rol(register) is one to the left also can be done with lsb(register)
            staa    pattern
            jsr		SPEAKER
            		
;        	ldaa	doubleLED
;       		adda	doubleLED
;        	staa	PTS
;        	cmpa	#128
;        	beq		RESET_LED_PLOW
;        	staa	doubleLED
;        	bra		continue_ON
;RESET_LED_PLOW:
;			movb	#01, doubleLED
;continue_ON:			
            pshd                                ;prepare to update and display field
            ldd     #disp
            jsr     display_string     
            puld
            ldaa  	disp_count                  ;disp_count = 0?
            dbeq    a, plowspace                ;if so display the spaces
            staa   	disp_count                  ;otherwise save decremented plow for next time
           	bra		plow7new                       ;continue to loop until disp_count = 0

plowspace:     
            ldaa    row_check                       
            cmpa    #2                              ;row_check = 2?
            beq     finalplow                       ;exit out of plowing and notify user
            movb    #' ', 1,y+                      ;spaces separates the first 7 '#' on the LCD's to the second 7 '#' of each row
            ldaa    space_check                     ;check if 2 spaces has been displayed
            dbeq    a, prepare_secondplow           ;if space_check =  0, reset variables
            staa    space_check                     ;otherwise save decremented space variable for next time
            bra     plowspace                       ;loop one more time to display ' '
            
prepare_secondplow:                                      
		    ldaa    row_check                       ;check to see if the second14 has already been displayed
		    cmpa    #1                              ;row_check = 1?
		    beq     last_plow	                    ;branch and incremenet row_check flag one last time
		    movb	#14, disp_count                 ;set disp_count for the second 14 '#'
		    movb    #2, space_check                 ;set
		    movb    #1, row_check                   ;row_check = 1
		    pshd
            ldd     #disp
           	jsr     display_string
            puld
                      
plow14:     movb    #$80,pattern                                        ; Displays the "middle" 14 '#' on the field
plow14new:  brclr   halfsec_pass, #$01, plow14       ;eternal loop until 0.5 second has passed
            clr     halfsec_pass                     ;reset "half second" flag
            movb	#'#', 1,y+                      ;display that one section of the field is plowed
            ldaa    pattern
            staa    PTS
            lsra
            staa    pattern
            jsr		SPEAKER
			            
cont_plow:
			pshd                                    ;prepare to update and display field
            ldd     #disp
            jsr     display_string     
            puld
            ldaa  	disp_count
            dbeq    a, plowspace                    ;if so display the spaces
           	staa    disp_count                      ;otherwise save decremented plow for next time
           	bra     plow14new                          ;continue to loop until disp_count = 0

last_plow:
            movb    #2, row_check                   ;row_check = 2
            movb    #7, disp_count                  ;reset disp_count for the last 7 #'s
            lbra    plow7
    
finalplow:      
            ;movb	#1, LCDwaitflag	                ;tell interrupt to delay for several seconds
            movb	#1, field_finished
            movb    #1, field                       ;field = 1 = field has been plowed
            clr     disp_count
            clr     row_check
            clr		space_check    
     	    movb 	#0,disp+32                         ;string terminator, acts like '\0'  
           	ldd 	#disp
           	jsr  	display_string
            
;*********************************FIELD HAS BEEN PLOWED*************************************
           	ldd  		#LCD_PLOWED
           	jsr  		display_string             
LOOP4:     	
		    jsr		KEYPAD
           	ldab	match
           	cmpb	#6
           	BNE		LOOP4
           	lbeq	Proceed
           
;************************CHECKING: FIELD CONDITION - PLOWED OR NOT**************************       
PlantSeed:
           	ldaa	field
           	cmpa	#1                         
           	lbeq	YouMayPass4
           	lbra	DENY_PLANTING	;deny user since field must be plowed first (fieldd = 0 currently)
           
;***************************STEPPER MOTOR: PLANT THE FIELD**********************************
YouMayPass4:
           	sei
            cli
            movb	#7, disp_count  ;set disp_count for the first 7 '.'
            movb    #2, space_check ;set space2 for the spaces between the '.'
            movb    #0, row_check   ;clear row_check to start checking positioning of the plowing process                             
            ldy     #disp      		;prepare to use disp to store and display '.' and ' ' accordingly
            movb    #7, field		;field = 7 means that planting is in progress
            jsr		SPEAKER			;SPEAKER: long_note
                        
;row_check:
; meant to check the pattern of the planting process 
                       
plant7:		movb    #$01,pattern
plant7new:                                             ; Displays the first and last 7 '.' on the field
            brclr   second_pass, #$01, plant7       ;eternal loop until 1 second has passed
            brclr   second_pass, #$01, plant7       ;eternal loop until 1 second has passed
            clr     second_pass                     ;reset "second" flag
            movb	#'.', 1,y+                      ;display that one section of the field is plowed
            ldaa    pattern
            staa    PTS
            rola								;lsr(register) is one shift to the right rol(register) is one to the left also can be done with lsb(register)
            staa    pattern
            jsr		SPEAKER
            pshd                                    ;prepare to update and display field
            ldd     #disp
            jsr     display_string     
            puld
            ldaa  	disp_count                      ;disp_count = 0?
            dbeq    a, plantspace                   ;if so display the spaces
           	staa    disp_count                      ;otherwise save decremented plow for next time
           	bra     plant7new                          ;continue to loop until disp_count = 0

plantspace:     
            ldaa    row_check                       
            cmpa    #2                              ;row_check = 2?
            beq     finalplant                      ;exit out of plowing and notify user
            movb    #' ', 1,y+                      ;spaces separates the first 7 '#' on the LCD's to the second 7 '#' of each row            
            ldaa    space_check                     ;check if 2 spaces has been displayed
            dbeq    a, prepare_secondplant          ;if space_check =  0, reset variables
            staa    space_check                     ;otherwise save decremented space variable for next time
            bra     plantspace                      ;loop one more time to display ' '
            
prepare_secondplant:                           
			ldaa    row_check                       ;check to see if the second14 has already been displayed
			cmpa    #1                              ;row_check = 1?
			beq     last_plantcheck	                    ;branch and incremenet row_check flag one last time
			movb	#14, disp_count                 ;set disp_count for the second 14 '#'
			movb    #2, space_check                 ;set
			movb    #1, row_check                   ;row_check = 1
			pshd
            ldd     #disp
            jsr     display_string
            puld
                      
plant14:	movb    #$01,pattern				
plant14new:                                            ; Displays the "middle" 14 '#' on the field
            brclr   second_pass, #$01, plant7       ;eternal loop until 1 second has passed
            clr     second_pass                     ;reset "second has passed" flag
            movb	#'.', 1,y+                      ;display that one section of the field is plowed
            ldaa    pattern
            staa    PTS
            rola								;lsr(register) is one shift to the right rol(register) is one to the left also can be done with lsb(register)
            staa    pattern
            jsr		SPEAKER
            pshd                                    ;prepare to update and display field
            ldd     #disp
            jsr     display_string     
            puld
            ldaa  	disp_count
            dbeq    a, plantspace                   ;if so display the spaces
           	staa    disp_count                      ;otherwise save decremented plow for next time
           	bra     plant14new                         ;continue to loop until disp_count = 0

last_plantcheck:
            movb    #2, row_check                   ;row_check = 2
            movb    #7, disp_count                  ;reset disp_count for the last 7 #'s
            lbra    plant7
    
finalplant:      
            ;movb	#1, LCDwaitflag	                ;tell interrupt to delay for several seconds
            movb	#1, field_finished
            movb    #2, field                       ;field = 2 = field has been planted
            clr		twentyonesec_pass				;clear out necessary times at this time
            clr		twentyone_sec					;to properly measure the growth of the crops
            clr		second							;from one stage to the next (field = 2->3->4)
            clr     disp_count
            clr     row_check 
            clr	    space_check   
     	    movb    #0,disp+32                         ;string terminator, acts like '\0'  
           	ldd     #disp
           	jsr     display_string
           	           	
;********************************FIELD HAS BEEN PLANTED*************************************
           	ldd  #LCD_PLANTED
           	jsr  display_string             
LOOP5:     	
			jsr		KEYPAD
           	ldab	match
           	cmpb	#7
           	bne		LOOP5
           	lbeq	Proceed

;**********************4) MENU SELECTION: FERTILIZE AND SPRINKLE****************************            	
;******************************DC MOTOR: PLANT THE FIELD************************************
DCMOTOR:
           	ldd  #menu3
		   	jsr  display_string        
LOOP6:     
			jsr		KEYPAD
			ldab	match
           	cmpb 	#8
           	lbeq	Fertilize
           	cmpb 	#9
           	lbeq	Irrigate
           	BNE		LOOP6
           	
;***************************DC MOTOR: FERTILIZE THE FIELD**********************************          
Fertilize:	    
		    ldd	    #fertInProgress
            jsr     display_string
            movb    #1, water			;water = 1 means that fertilizing in progress,
            movb	#$0F, pattern
            ;movb	pattern, PTS
LOOP7:     
			brset   qrtsec_pass, #$01, led1
			jsr		KEYPAD
			ldab	match
           	cmpb 	#$A
           	lbeq	DoneWDC
            jsr		SPEAKER	
            Bra		LOOP7
            
led1        clr		qrtsec_pass
			;movb	pattern, PTS		;preparing the pattern for 
			;ldaa    pattern
			ldaa	PTS
			eora	#$FF
            ;staa    PTS
            ;rola
            ;rola
            ;rola
            ;rola								;lsr(register) is one shift to the right rol(register) is one to the left also can be done with lsb(register)
            ;staa    pattern
            staa	PTS
            bra 	LOOP7           	

;***************************DC MOTOR: IRRIGATE THE FIELD***********************************            	 
Irrigate:	ldd		#irrInProgress
	        jsr  	display_string
            movb    #2, water		;water = 2 means that irrigation in progress,
            movb	#$0F, pattern
            movb	pattern, PTS 				
LOOP8:     
			brset   qrtsec_pass, #$01, led2
			jsr		KEYPAD
			ldab	match
           	cmpb 	#$B
           	lbeq	DoneWDC
           	jsr		SPEAKER
           	Bra		LOOP8
           	
led2        clr		qrtsec_pass
			ldaa	PTS
			eora	#$FF
            staa	PTS
            bra 	LOOP8                      	
           	
;************************DC MOTOR: FIELD ALTERATION COMPLETE*******************************           	
DoneWDC:	movb	#0, portS			; Need to restore irrigation and LED to their former state
			movb	#0, water
			bclr	Port_T, #$08
            lbra	Proceed
            
;irrigate is one of the flags
;need to setup another flag for the irrigation process to change when the switches inform the user     
           	
;*******************************************************************************************************************
;*****************************ACCESS DENIED/INVALID USER INPUT/CLARIFY**********************************************
Denied:
			;brclr   PTP, #%00100000, PUSH			;CHECKS PUSH BUTTON EVERYWHERE		
		    ldd		#DENY_MSG
           	jsr		display_string         
LOOP00:     
			;brclr   PTP, #%00100000, PUSH			;CHECKS PUSH BUTTON EVERYWHERE
		    jsr		KEYPAD
           	ldab	match
           	cmpb	#0
           	lbeq	Proceed
           	bra		LOOP00               
            
DENY_PLANTING:
           	ldd  #DENY_PLANT
           	jsr  display_string
LOOP01:     
			;brclr   PTP, #%00100000, PUSH			;CHECKS PUSH BUTTON EVERYWHERE
			jsr		KEYPAD
           	ldab	match
           	cmpb	#0
           	lbeq	Proceed
           	bra		LOOP01           
           	
CLARIFY_PLOW:
           	ldd  #PLOW_AGAIN
           	jsr  display_string
 LOOP02:
 			;brclr   PTP, #%00100000, PUSH			;CHECKS PUSH BUTTON EVERYWHERE
 			jsr		KEYPAD 			
           	ldab	match
           	cmpb	#1
           	lbeq	YouMayPass3	;return to plowing the field
           	cmpb	#2
           	lbeq	Denied      ;goes to display denied access
           	bra		LOOP02

;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************INTERRUPT SUBROUTINE****************************************************
RTI_ISR:									;Checks for every 1ms
								 
			ldab	PTP						;brclr   PTP, #%00100000, PUSH	 ;Check PushButton   ;20 in hex                                     
			orab	#%11011111
			cmpb	#%11011111
			lbeq	PUSH
			
		    ;ldab	match					;Check for user input of "E"
		   	;cmpb	#$E						
		    
		    
	        ;figure out how to make if the PUSH button is pressed
		; if pressed jump to separate routine + making sure the portP configuration with the stepper motor is undistrubed
		; button NOT PRESSED, continue on with checking field flag and switches		
;*******************************************************************************************************************
;********************************SOIL SETTING: FERTILITY + HUMIDITY CHECKS******************************************	        
 	        brset   PTT, #$01, checkbit1     ;Check switch 1 bit 0 which when set checks for fertility condition
	        brset   PTT, #$02, checkbit0      ;Check switch 2 bit 1 which when set checks for humidity condition
 			lbra	checkcases
 			
checkbit1:  ldab	PTT						;brclr	PTT, #$02,FertilitySetting                                    
			orab	#%11111101
			cmpb	#%11111101
			lbeq	fertilizing0
											
checkbit0:	ldab	PTT						;brclr	PTT, #$01,HumiditySetting
			orab	#%11111110
			cmpb	#%11111110
			lbeq	irrigation0									

;FertilitySetting:
;			ldaa	fertilizer			;BY DEFAULT: soil is unfertile		if we want to decrease the speed too
;			cmpa	#0					;if not fertilized, display the unfertile message
;			lbeq	fertilizing0
;			cmpa	#1					;if so, display the fertile message
;			lbeq	fertilizing1
		
;HumiditySetting:
;			ldaa	irrigate			;BY DEFAULT: soil is dry
;			cmpa	#0					;if not humid, display the dry message
;			lbeq	irrigation0
;			cmpa	#1					;if so, display the humid message
;	   		lbeq	irrigation1




;*******************************************************************************************************************
;**************************MOTOR SETTING: STEPPER = FIELD CHECKS + DC = SOIL CHECKS*********************************	        
continue:		
			ldaa	field
			cmpa	#0
			lbeq	welcome_LED
;Stepper_Motor
checkcases:
		    ldaa	field
		    cmpa	#6					
		    lbeq	plowing 			; field = 6 = stage of plowing
		    cmpa	#7
		    lbeq	planting			; field = 7 = stage of planting
		    cmpa	#8					
		    lbeq	harvesting			; field = 5 = stage of harvesting         
;DC MOTOR path	
            ldaa	water	    
		    cmpa	#1
		    lbeq	fertilizing			; field = 8 = process of fertilizing
		    cmpa	#2					
		    lbeq	irrigation			; field = 5 = process of irrigation???? (Need to do research)
		    ;lbra	EXIT_RTI 
		
;*******************************************************************************************************************
;*********************GROWTH STAGES: CHECKING THE STATUS OF THE FIELD BASED ON TIME*********************************
checkstage0:	
			ldaa	stageflag
			cmpa	#1
			bne		setstage
			ldaa 	threesec_flag	;21 seconds passed? FOR NOW IT'LL BE 3 SECONDS
			cmpa	#1
			beq		checkstage1
			lbra 	EXIT_RTI

checkstage1:
			;movb	#$03, PTS		;debugging purposes to signify the growth stage is in checking session
			;clr		threesec_flag
			ldaa	field
			cmpa	#2				;if field = 2(planeted), change from planted -> growing
			beq		grow1
			cmpa	#3				;if field = 3(growing), check for humidity and fertility
			beq		grow2			
			cmpa	#4				;if field = 4(maturity), change from maturity -> harvestable
			beq		grow1
			lbra	EXIT_RTI		;if field is neither cases, exit out of the routine
									;Maybe if field is 5 it should toggle a notification on the lcd saying press pb to harvest
									;Need to reset the flags for next time????
									;only times is when field = 5 when its harvestable

grow1:								
			inc 	field			;grow the field into the next stage
			clr		stageflag		;clear out the setstage so the next entry will clear out seconds for appropriate growth timing
			ldaa	field
			cmpa	#5
			beq		LED_NOTE
			;movb	#$3C, PTS		;debugging that field has incremented to the next stage
			lbra 	EXIT_RTI
LED_NOTE:
			movb	#$81, PTS
			lbra	EXIT_RTI			

grow2:	
			ldaa 	irrigate		;check whether the field is humid
			cmpa	#1
			beq 	checkfert		;if field is humid, check if the field is fertile
			;lbra 	grow1
			lbra 	EXIT_RTI		;otherwise, exit out of subroutine until field is humid to continue
	
checkfert:
			ldaa 	fertilizer		;check whether the field is fertile
			cmpa	#1
			beq 	maturity		;if field is fertile, incremenet the field from growing -> maturity
			lbra 	EXIT_RTI		;otherwise, exit out of subroutine until field is fertile to continue
			
maturity:
			ldx		#disp
next_element:
			ldaa	0,x				;the individual elements will be pulled out and converted accordingly
			cmpa	#0				;condition 1: is the array at the end?
			lble	grow1			;if so, branch to change growth stage of field
			cmpa	#$20			;condition 2: is the array element at the ' '?
			beq		movealong		;if so, skip over the element and move onto the next element
			adda	#18			;convert the ascii value of '@' to '$'
movealong:
			staa	1, x+			;store and move on to the next part of the array
			bra		next_element	;repeat until the array has reached its end						

			
setstage:
			clr		second			;clear out all necessary flags for the growing process to work out
			clr		second_pass
		    clr		threesec_flag
       		clr		twentyone_sec
        	clr		twentyonesec_pass
			movb	#1, stageflag
		    lbra	EXIT_RTI			;if neither PB, setting changing switches are pressed NAND none of the motor stimulating field stages are active, exit RTI
 welcome_LED:
 			jsr			LED_Light
 			lbra		checkcases
				
						   
;*******************************************************************************************************************
;**************************************PUSH BUTTON: CHECKING FIELD FLAG*********************************************
PUSH:          		   
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
		    ;cmpa	    #5              ;is the field harvestable?
		    ;beq		    READY_FIELD	
		    lbra		EXIT_RTI        ;otherwise exit
          		   
;*******************************************************************************************************************
;********************************PUSH BUTTON: FIELD CONDITION DISPLAYED ON LCD**************************************          		   
EMPTY_FIELD:
	      	ldd		    #CropCondition0
		    jsr		    display_string
		    ;ldd		    #CropCondition0
		    ;jsr		    display_string
		    lbra		EXIT_RTI

PLOWED_FIELD
		ldd		#CropCondition1
		jsr		display_string
		;ldd		#CropCondition1A
		;jsr		display_string
		lbra		EXIT_RTI	   

SOWED_FIELD
		ldd		#CropCondition2
		jsr		display_string
		;ldd		#CropCondition2A
		;jsr		display_string
		lbra		EXIT_RTI	 
		
GROWING_FIELD
		ldd		#CropCondition3
		jsr		display_string
		;ldd		#CropCondition3A
		;jsr		display_string
		lbra		EXIT_RTI

MATURE_FIELD
		ldd		#CropCondition4
		jsr		display_string
		;ldd		#CropCondition4A
		;jsr		display_string	
		lbra		EXIT_RTI
		
;READY_FIELD
		;ldd		#CropCondition5A
		;jsr		display_string
		;brclr   PTP, #%00100000, harvesting
		;;ldd		#CropCondition5A
		;;jsr		display_string
		;lbra		EXIT_RTI
;*******************************************************************************************************************
;*******************************************FIELD CASE: HARVESTING**************************************************		
harvesting:				    						     
		ldaa		field_finished			;field is done harvesting until the flag = 1
		cmpa		#1					    ;if so the harvesting will end
		beq			HARVEST_OFF			    ;and turn off the stepper motor
		;movb		#6, portS				; LED DISPLAYS: 6 signifying it is plowing
		ldaa		harv_var            	;check if the plow_var 
		dbne		a, STORE_HARV	
		movb		#150, harv_var           ;store back the value to the (fast) speed variable for plowing		
		
		ldx			anticlock_temp			;load accumulator x with counterclockwise direction address
		cpx			#anticlockwise+4
		bne			continue_harv			;continue on to load value at counterclockwise array address
		ldx			#anticlockwise
		stx			anticlock_temp
continue_harv:	
		ldx			anticlock_temp	
		ldab    	0,x                		;anticlockwise values in b
	    inx		                   			;reset
	    stab		port_p		        	;counterclockwise fast loop
	    stx			anticlock_temp
	    lbra		EXIT_RTI
	       
STORE_HARV:
		staa		harv_var
		lbra		EXIT_RTI

HARVEST_OFF:
		movb		#0, field_finished
		lbra		EXIT_RTI	    						       		   
		
;*******************************************************************************************************************
;********************************************FIELD CASE: PLOWING****************************************************
plowing:		
		ldaa		field_finished			;field is done plowing until the flag = 1
		cmpa		#1					    ;if so the plowing will end
		beq			PLOW_OFF			    ;and turn off the stepper motor
		;movb		#6, portS				; LED DISPLAYS: 6 signifying it is plowing
		ldaa		plow_var            	;check if the plow_var 
		dbne		a, STORE_PLOW	
		movb		#50, plow_var           ;store back the value to the (fast) speed variable for plowing		
		
		ldx			anticlock_temp			;load accumulator x with counterclockwise direction address
		cpx			#anticlockwise+4
		bne			continue_plow			;continue on to load value at counterclockwise array address
		ldx			#anticlockwise
		stx			anticlock_temp
continue_plow:	
		ldx			anticlock_temp	
		ldab    	0,x                		;anticlockwise values in b
	    inx		                   			;reset
	    stab		port_p		        	;counterclockwise fast loop
	    stx			anticlock_temp
	    lbra		EXIT_RTI
	       
STORE_PLOW:
		staa		plow_var
		lbra		EXIT_RTI

PLOW_OFF:
		movb		#0, field_finished
		lbra		EXIT_RTI		       		
		
;*******************************************************************************************************************
;*******************************************FIELD CASE: PLANTING****************************************************
planting:		
		ldaa		field_finished			;field is done planting until the flag = 1
		cmpa		#1					    ;if so the planting will end
		beq			PLANT_OFF			    ;and turn off the stepper motor
		;movb		#7, portS				;LED DISPLAYS: 7 signifying it is planting
		ldaa		plant_var				;plant_var holds the "speed" value of the stepper motor and decrement
		dbne		a, STORE_PLANT			;if not at 0, the value will be saved back and will exit out the RTI

		movb		#150, plant_var         ;store back the value to the (slow) speed variable for plowingplant_var

		ldx			clockwise_temp			;load accumulator x with clockwise direction address
		cpx			#clockwise+4			;checks if the address points to the last element and
		bne			continue_plant			;if not, branch out
		ldx			#clockwise				;if at the last element, reset clockwise address
		stx			clockwise_temp			;and store it back to clockwise_temp
continue_plant:	
		ldx			clockwise_temp	
		ldab    	0,x                		;clockwise values in b
	    inx		                   			;clockwise_temp incremenets address value
	    stab		port_p		        	;values in b is sent to Port_P to move the stepper motor
	    stx			clockwise_temp			;save incremeneted address and prepare to exit our of RTI
	    lbra		EXIT_RTI
	       
STORE_PLANT
		staa		plant_var
		lbra		EXIT_RTI

PLANT_OFF:
		movb		#0, field_finished
		lbra		EXIT_RTI		       		            


;*******************************************************************************************************************
;********************************SOIL SETTING: FERTILITY + HUMIDITY DISPLAYS****************************************
;INFERTILE SOIL
fertilizing0:	
			ldd  	#infertile			;prepare to display the appropriate messages
           	jsr  	display_string
			movb 	#1, fertilizer
			lbra		EXIT_RTI				
;FERTILE SOIL
;;fertilizing1:
		;;	ldd  	#fertile			;prepare to display the appropriate messages
        ;;   	jsr  	display_string
		;;	movb 	#0, fertilizer	
;DRY SOIL
irrigation0:  
			ldd  	#dry				;prepare to display the appropriate messages
           	jsr  	display_string
 			movb 	#1, irrigate
 			lbra		EXIT_RTI
;HUMID SOIL 
;;irrigation1:
;;			ldd  	#humid				;prepare to display the appropriate messages
;;           	jsr  	display_string
;;			movb 	#0, irrigate

;*******************************************************************************************************************
;***************************************DC MOTOR: FERTILITY % CYCLES************************************************	
;10% DUTY CYCLE
fertilizing:
            ldaa       	fertilizer			;soil fertile (1) or not (0)
            cmpa       	#1					;if fertile, change DC Speed to 50%
            beq			fertilizing2        
            movb		#23, DC_Speed	    ;otherwise, set DC Speed to 10% ;22% is lowest possible to spin on its own
			bra			DCMOTOR1			;branch to spin the DC motor accordingly

;50% DUTY CYCLE			
fertilizing2:
			movb		#45, DC_Speed	    ;set DUTY CYCLE to 50% since soil is fertile
			bra			DCMOTOR1			;branch to spin the DC motor accordingly	
			
;*******************************************************************************************************************
;****************************************DC MOTOR: HUMIDITY % CYCLES************************************************						
;50% DUTY CYCLE
 irrigation:	
            ldaa        irrigate                       ;flag that indicates whether irrigation speed is high (2) or low (1)
            cmpa        #1								;if it is 1, that means go to the higher speed
            beq         humidity
            movb		#45, DC_Speed	     ;50%		;if not its 0 so just move regular irrigation speed to dcflag
			bra			DCMOTOR1
;100% DUTY CYCLE
humidity:	
            movb		#100, DC_Speed	     ;100%	
			bra			DCMOTOR1	
;EACH time the RTI does the cycle, it increments counter until 15 and then it'll clear the Port_t #$08
;reset the counter to 0 and 
; end of loop or not? reload t_on		  
;PUSH BUTTON PORT_P, SETTING/CLEARING BIT 5		 

;*******************************************************************************************************************
;*****************************DC MOTOR: IMPLEMENTATION OF % CYCLE ROTATION******************************************
DCMOTOR1: 
			ldaa		dcflag				;A has flag value
            cmpa    	#2					;IF dcflag = 2, 
            beq     	off0				;checks T_OFF
            cmpa    	#1					;IF dcflag = 1,
            beq     	on0             	;checks T_ON

; First section reads in new values of T_ON and calculates T_OFF:
            ldaa 		DC_Speed         	;otherwise, reads a new value of DC_Speed
            staa     	T_on				;and store that value into T_on
            ldab		#100		       	;prepare for subtracting T_on to get T_off
		    subb		T_on				;compute the difference and store the result
		    stab 	    T_off	       		;into the T_off variable
		    movb    	#1, dcflag       	;alter flag so that the RTI jumps to "on0"
		    ;lbra		EXIT_RTI
		    ;movb		#%11110000, PTS
		             
; Second section checks T_ON value:
on0:        ldaa    	T_on                ;prepare comparison if T_on
            cmpa    	#0                  ;is equal to 0
	        beq     	inc_fg0             ; check T_OFF if T_ON = 0
		    bset		Port_T, #$08	    ; set bit 3 of Port_T                                                
	        dec     	T_on                ; dec T_ON until 0       
	        lbra     	EXIT_RTI            ; exit routine + check T_ON again later
inc_fg0:    movb    	#2, dcflag          ; allows only fourth section to be executed
			;lbra		EXIT_RTI
			;movb		#%00001111, PTS
; Fourth section checks T_OFF value:
off0:	    ldaa    	T_off               ; is T_OFF = 0?
			
            cmpa	    #0		
		    beq 		dec_fg0	            ; clear FLAG for new T_ON/OFF
		    bclr		Port_T, #$08	    ; clear bit 3 of Port_T
		    dec     	T_off               ; dec T_OFF until 0
	        lbra     	EXIT_RTI			; exit routine + check T_OFF again later
dec_fg0:    movb    	#0, dcflag			; reset for new T_ON/OFF
			lbra		EXIT_RTI
						
;RTI for speaker

;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************EXITING PREPARATIONS****************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
EXIT_RTI:
		ldx     count_RTI           ;counts # of times RTI has performed
		inx
		stx     count_RTI
		inc		countRTI_flag		;IS THIS MEANT FOR THE SPEAKER???
		ldaa	countRTI_flag
		cmpa	#250
		bne		continueon
		clr		countRTI_flag
continueon:		
		cpx     #1953               ;counts for 1/4th of a second
		beq     INC_QTSEC
		cpx		#3906               ;counts for 1/2 of a second
		beq		INC_HALFSEC
		cpx     #5859               ;counts for 3/4th of a second
		beq     INC_THREEFOURSEC
		cpx		#7812               ;counts for a second
		beq		INC_SEC
		lbra    EXIT		
		
INC_QTSEC:
        movb    #1, qrtsec_pass     ;set flag for 0.25 second
        ldaa	qrtsec_song			;counter of 1/4 second for song
        inca
        staa	qrtsec_song
        lbra     EXIT						
INC_HALFSEC:	
		movb    #1, qrtsec_pass     ;set flag for 0.25 second
		ldaa	qrtsec_song			;counter of 1/4 second for song
        inca
        staa	qrtsec_song
		movb	#1, halfsec_pass 	;set flag for 0.5 second
		lbra     EXIT                
INC_THREEFOURSEC:
        movb    #1, qrtsec_pass     ;set flag for 0.25 second
      	ldaa	qrtsec_song			;counter of 1/4 second for song
        inca
        staa	qrtsec_song
        lbra     EXIT        		
INC_SEC:
        movb    #1, qrtsec_pass     ;set flag for 0.25 second
        ldaa	qrtsec_song			;counter of 1/4 second for song
        inca
        staa	qrtsec_song	
	    movb	#1, halfsec_pass    ;set flag for 0.5 second
		movb	#1, second_pass     ;set flag for 1 second
		movw    #00, count_RTI      ;resetting RTI counter
		inc     second				;counter for amount of second(s) passed
		
		ldaa    second				;prepare to check if three seconds passed
		cmpa	#3
		beq     INC_THREESEC 		;set and clear flags accordingly after three seconds
		lbra     EXIT		
		
INC_THREESEC:
       	movb    #1, qrtsec_pass
       	movb	#1, halfsec_pass
       	movb	#1, second_pass
        movb    #1, threesec_flag
        ;clr		second
        inc		twentyone_sec
        ldaa	twentyone_sec
;        cmpa	#7
 ;       beq		INC_TWENTYONESEC
        lbra     EXIT

;;INC_TWENTYONESEC:
;;		clr		twentyone_sec	
;;		inc		twentyonesec_pass
;;		ldaa	twentyonesec_pass
;;		cmpa	#1
;;		beq		CHANGE_FIELD1
;;		cmpa	#2
;;		beq		CHANGE_FIELD2
		
CHANGE_FIELD1:
		bra		EXIT
CHANGE_FIELD2:
		clr		twentyonesec_pass   
		     	         			
EXIT:

		;enter PlayTone as long as Pest Event is active
			;brclr		IRQflag, $01,  continue
		;;	ldaa		IRQflag
	;;		cmpa		#1
	;;		bne			ex2
;;			jsr 		PlayTone	
ex2:		movb	#$80, CRGFLG
		RTI	
        ;ldaa     qrt_sec             ;increment # of times 1/4th of second passed
        ;inca
        ;staa     qrt_sec
        ;movw    qrt_sec, PTS;debug     ;set flag for 0.25 second		
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;*******************************************************************************************************************
;********************************END OF REAL-TIME INTERRUPT SUBROUTINE**********************************************


;*******************************************************************************************************************
;*******************************************************************************************************************
;*********************************************IRQ SUBROUTINE********************************************************
IRQ_PEST:
			;A, B, X, Y, CCR, RA(PC)	= 9 bytes
			;leas 	9, sp
			cli
			movb	#1, IRQflag			;set flag to let all outputs stop moving
			
			; CLEAR OUT ANY NECESSARY OUTPUTS
            movb	#$00, PTS			;clear out any LED lights
            
			; STOP ALL MOTORS AND RECALL PREVIOUS FIELD AND WATER VARIABLES
		    movb	#0, water			;clear out water variable + prevents entry to DC motor
			movb	#0, dcflag			;clears entry to DC motor
            bclr	Port_T, #$08		;clears the DC motor
            ldaa	field				;checks previous value of field
            cmpa	#6
            beq		RESET_PLOW			;field was in plowing process so reset accordingly
            cmpa	#7
			beq		RESET_PLANT			;field was in planting process so reset accordingly
			cmpa	#8
			beq		RESET_HARVEST		;field was in harvesting process so reset accordingly
			lbra	CLEAR_SEC			;otherwise, branch to clearing the seconds
			
			; RESET FIELD FLAGS ACCORDINGLY			
RESET_PLOW:
			movb	#0, field			;field was plowing so reset back to field being empty
			lbra	CLEAR_SEC
RESET_PLANT:
			movb	#1, field			;field was planting so reset back to field being plowed
			lbra	CLEAR_SEC
RESET_HARVEST:
			movb	#5, field			;field was harvesting so reset back to field being harvest ready
			lbra	CLEAR_SEC  
			         
            ; RESET ALL OF THE SECONDS
CLEAR_SEC
        	movb    #0, qrtsec_pass
        ;	movw    #00, qrt_sec
        	movb	#0, halfsec_pass
        	movb    #0, second_pass
        	movb	#0, second
        	movb    #0, threesec_flag 
            
			; prepare to clear out the LCD screen                  
            ldd		#pests				;prepares to display the pest message to LCD screen
            jsr		display_string
                   	
;hold out the message for three seconds until it displays pot_screen
holdthreesec:
			
			jsr		LED_Light
            brclr   threesec_flag, #$FF, holdthreesec 
            
            
;*******************************************************************************************************************
;********************************POTENTIOMETER: DISPLAYING AMOUNT OF PESTICIDE**************************************
loop1: 	;PREPARE FIRST DIVISION TO CALCULATE AND STORE VALUE FOR HDIG	
		jsr 	read_pot		;subroutine that reads from potentiometer.c file
 		ldd 	pot_value		;value of the pot into D
 		ldx		#100			;100 into X
 		idiv 					;divide the pot value by 100
 		pshd					;save remainder to the stack						    
 		tfr		x, a			;move quotient in X to A to properly transfer value to hDig
 		adda	#$30			;add $30 to a to get proper ascii value
 		staa	hDig			;quotient value stored in hundreds place
 		puld					;pull remainder back to D

		;PREPARE SECOND DIVISION TO CALCULATE AND STORE VALUES FOR TDIG AND ODIG
 		ldx 	#10 			;10 in x
 		idiv 					;divide pot value by 10
 		addd 	#$30			;add $30 to remainder (in D) for proper ascii value
 		stab 	oDig			;store remainder value to ones place
 		tfr		x, a			;get second quotient from X to A for proper transfer to tDig
 		adda	#$30			;add $30 to second quotient (in A) for proper ascii value
 		staa	tDig			;store the value to tDig
 		   		
 		;DISPLAY PREPARATION + CHECKING VALUE OF HUNDEREDS PLACE
 		movb hDig, Pest_pot+25	;hDig = Hunderds Digit to be displayed		 This doesnt work
 		movb tDig, Pest_pot+26	;tDig = Tens Digit to be displayed
 		movb oDig, Pest_pot+27 	;oDig = Ones Digit to be displayed
		ldd #Pest_pot 			;Display the LCD screen with % of pesticide
 		jsr display_string
 		ldaa hDig				;check the hundreds place and if value = 1
 		cmpa #$31				;then pesticide level reached 100%+
 		lbne loop1				;if below 100%, continue to read the pot
 		ldd #pestkill 			;otherwise, display message that all pests are killed after
 		jsr display_string		;100%+ amount of pesiticide has been used
 		
 		;RESET ALL OF THE SECONDS
        movb    #0, qrtsec_pass
        ;movw    #00, qrt_sec   
        movb	#0, halfsec_pass
        movb    #0, second_pass
        movb	#0, second
        movb    #0, threesec_flag
        
holdthreesec2:
            brclr   threesec_flag, #$FF, holdthreesec2	;3 second wait to display the pestkill message
IRQ_WAIT:
            movb	#0, IRQflag							;clear the IRQflag to enable all other external outputs
            movb	#$80, CRGFLG						;proper exiting of the IRQ routine
            rti						;branch back to the first menu