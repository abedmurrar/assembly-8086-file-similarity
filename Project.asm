
;ABED AL RAHMAN MURRAR 1140155
;FAHED JBARA 1151736 
;ENCS311 LAB ASSEMBLY PROJECT
;OBJECTIVE : FIND SIMILARITY PERCENTAGE IN STRING IN TWO FILES
.MODEL TINY ; SELECT TINY MODEL   

.DATA

    FILE_1 DB "FILE1.TXT", 0 
    HANDLEF_1 DW 0
    FILE_2 DB "FILE2.TXT", 0
    HANDLEF_2 DW 0 
    										 
    BUFFER1 DB 200 DUP (0) 
    BUFFER2 DB 200 DUP (0) 
    
    LENGTH1 DW ?
    LENGTH2 DW ?
    
    NO_WORDS1 DW ?
    NO_WORDS2 DW ?
    
    LAST_ADDRESS DW ?
    TEMP DB 15 DUP (0)
    
    ERROR_MSG DB "ERROR IN READING FILE!",'$'
    SIM_MSG DB "SIMILARITY BETWEEN THE TWO FILES IS : ",'$'
    
    INTERSECTION	DW ?													  
    UNION			DW ?
    
    RESULT DW ?			   
    
    STOP_WORDS DW S0,S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11
    STOP_WORDS_SIZE=($-STOP_WORDS)/2
    			S0  DB "that",'$'
    			S1  DB "an"  ,'$'
    			S2  DB "as"  ,'$'
    			S3  DB "at"  ,'$'
    			S4  DB "the" ,'$'
    			S5  DB "by"  ,'$'
    			S6  DB "in"  ,'$'
    			S7  DB "for" ,'$' 
    			S8  DB "of"  ,'$' 
    			S9  DB "on"  ,'$' 
    			S10 DB "a"   ,'$'
    			S11 DB "i"   ,'$'
    
    PUNCTUATION_MARKS DB '.','?','!',':','-',';','_','(',')',','
    PUNC_MARKS_SIZE=($-PUNCTUATION_MARKS)
    					  
    
.CODE ; INDICATE START OF CODE SEGMENT
    JMP START		
    ;***********************************************************
    PRINT_NUM PROC NEAR
    		;PASS NUM THROUGH AX
    		PUSHA
    		XOR CX, CX
    		MOV BX, 10D
    	
    	HEX_TO_DEC:
    		XOR DX, DX
    		DIV BX ;DIVIDE DX:AX BY 10 && AX = DX:AX / 10  
    		PUSH DX;SAVE REMAINDER
    		INC CX ;FOR THE LOOPS TO PRINT DIGITS
    		TEST AX,AX ; AX = AX / RADIX_OUTPUT
    			JNZ HEX_TO_DEC
    		
    		NUM_TO_ASCII:
    		POP DX
    		MOV AH,02H
    		ADD DL, 30H ;CONVERT IT TO ASCII
    			CMP DL, 39H
    			JBE SKIP
    			ADD DL, 7H
    		SKIP:	 
    		INT 21H ;SHOW THE DIGIT ON SCREEN
    		LOOP NUM_TO_ASCII ;REPEAT FOR ALL DIGITS  
    		
    		MOV AH, 02H
    		MOV DL, '%'
    		INT 21H
    		
    		POPA 
    		RET
    PRINT_NUM ENDP
    ;***********************************************************
    PRINT_MSG MACRO MSG
    	PUSHA
    	MOV AH,9
    	LEA DX, MSG
    	INT 21H
    	POPA
    ENDM
    ;*********************************************************** 
    OPEN_FILE MACRO FILENAME, FILEHANDLER
    	PUSHA
    	MOV AH,3DH ; OPEN A FILE
    	MOV AL,02
    	LEA DX,FILENAME ; THE ADDRESS OF FILE NAME SHOULD BE IN DX
    	INT 21H  
    	MOV FILEHANDLER,AX 
    	POPA
    ENDM	   
    ;***********************************************************  
    CLOSE_FILE MACRO FILEHANDLER 
    	PUSHA
    	MOV BX, FILEHANDLER
    	MOV AH,3EH
    	INT 21H		 
    	POPA
    ENDM		 
    ;***********************************************************
    READ_FROM_FILE MACRO FILEHANDLER, BYTES, VARIABLE, LENGTH 
    	PUSHA   
    	LOCAL L, EXIT, ERROR 
    		XOR SI,SI
    		MOV AH,3FH ; READ FROM FILE
    		MOV BX, FILEHANDLER 
    	L:
    		MOV CX,BYTES ; NUMBER OF BYTES TO BE READ
    		LEA DX,VARIABLE+SI ; THE ADDRESS OF DATA TO BE READ
    		INT 21H
    		CMP AX,0
    		JE  ERROR
    		JMP EXIT					  
    	ERROR:			 
    		PRINT_MSG ERROR_MSG
    		JMP TERMINATE
    	EXIT:
    		MOV LENGTH, AX
    		MOV SI, AX   
    		MOV BYTE PTR VARIABLE+SI,"$"
    		INC SI
    		MOV BYTE PTR VARIABLE+SI,'$' 
    	POPA
    ENDM
    ;*********************************************************** 
    CLEAN_STOP_WORDS MACRO BUFFER, LENGTH
    	PUSHA
    	LOCAL L,FINISH, L_NE,L2,EQUAL,DEC_LENGTH,NEXT_STOP_WORD
    	MOV AX,STOP_WORDS_SIZE ;NUMBER OF STOP WORDS
    	XOR BP,BP ;POINTER IN THE STOP WORD 
    	MOV BX, [STOP_WORDS+BP] ;ADDRESS OF A STOP WORD
    	L2:
    		LEA SI, BUFFER 
    		MOV CX, SI ;ADDRESS OF THE START OF THE BUFFER
    		ADD CX, LENGTH ;ADDRESS OF THE START OF BUFFER + LENGTH = LAST ADDRESS IN BUFFER
    		MOV LAST_ADDRESS, CX
    		MOV DI, BX ;DI CONTAINS THE ADDRESS OF THE STOP WORD
    		GET_LENGTH DI ;CX NOW EQUALS THE LENGTH OF THE STOP WORD
    		JMP L  
    	L_NE:
    		POP CX
    		POP DI
    	L:  
    		;CHECK IF SI DID NOT EXCEED THE LAST ADDRESS OF BUFFER
    		CMP SI, LAST_ADDRESS
    		JAE NEXT_STOP_WORD 
    		CLD																	
    		PUSH DI 
    		PUSH CX
    		;STORE THE STARTING ADDRESS OF THE STOP WORD AND ITS LENGTH 
    		;BECAUSE WE'RE GONNA REPEAT COMPARING IT TO THE BUFFER WHILE THE BUFFER IS ITERATING
    		 
    		REPE CMPSB
    		JNE L_NE
    		;IF EQUAL THEN
    		EQUAL: 
    		POP CX ;CX = LEGNTH OF STOP WORD 
    		POP DI ;DI = START ADDRESS OF STOP WORD
    		
    			PUSH SI
    				SUB SI, CX
    				MOV DX, OFFSET BUFFER
    				CALL CHECK_PREV
    			
    			POP SI
    			CMP DH, 1
    			JE L
    			CALL CHECK_NEXT
    			CMP DH, 1
    			JE L
    		
    		PUSH SI ; SI = ADDRESS(BUFFER) WHERE THEY'RE EQUAL + CX(ITERATIONS)
    		PUSH DI
    		PUSH CX				
    		
    			MOV DI, SI ;DI = ADDRESS(BUFFER) WHERE THEY'RE EQUAL + CX(ITERATIONS)
    			INC DI ; MOVE THE NEXT WORD IN PLACE OF THE REPLACED WORD
    			SUB SI, CX ;SI = ADDRESS(BUFFER) WHERE THEY'RE EQUAL
    			
    			MOV CX, LAST_ADDRESS
    			ADD CX, 2 ;INCLUDE THE LAST "$$" CHARACTERS
    			SUB CX, DI ;CX = LENGTH OF REMAINING CHARACTERS + "$$" AFTER THE TWO STRINGS MATCH
    			XCHG SI, DI
    			REP MOVSB ;COPY THE CHARACTERS AFTER THE MATCH ON THE MATCH AND CARRY ON	
    		
    		POP CX ; CX = LENGTH OF STOP WORD		 
    		POP DI ; DI = START ADDRESS OF STOP WORD
    		POP SI ; SI = ADDRESS(BUFFER) WHERE THEY'RE EQUAL + CX(ITERATIONS)
    		
    		PUSH CX ; STORE THE LENGTH OF STOP WORD BECAUSE IT WILL BE USED IN THE ITERATION
    			DEC_LENGTH:		
    				DEC LAST_ADDRESS ;LAST ADDRESS IS DECREMENTED 
    				DEC LENGTH ;AFTER WE CUT OFF THE STOP WORD FROM THE BUFFER; ITS LENGTH SHRINKED
    				DEC SI ;POINT AT THE CHARACTER THAT REPLACED THE STOP WORD
    				LOOP DEC_LENGTH
    		POP CX ; LOAD THE VALUE BECAUSE IT WILL BE RE-USED AGAIN FOR THE REST OF THE BUFFER
    		JMP L
    		NEXT_STOP_WORD: ;IF SI == LAST_ADDRESS, THEN WE FINISHED THE BUFFER AND NEED A NEW STOP WORD TO SEARCH FOR
    			DEC  AX ;NUMBER OF STOP WORDS REMAINING
    			INC  BP ; TO POINT AT THE ADDRESS OF THE NEXT STOP WORD
    			CMP  AX,0
    			JE   FINISH ;IF NO STOP WORDS REMAINING; FINISH
    			PUSH BP
    				ADD  BP, BP ;SINCE THEY ARE STORED AS DWORDS WE NEED TO MULTIPLY THE VALUE BY 2
    				MOV  BX,[STOP_WORDS+BP]
    			POP  BP		  
    			JMP  L2
    		
    	FINISH:
    		POPA
    ENDM
    ;***********************************************************
    PROC CHECK_PREV
    	;GETS STRING FROM SI
    	;DX = START OF STRING
    	;RETURNS IN DH
    	; IF DH==1 THEN IT'S A CHARACTER
    	PUSH SI
    	CMP SI, DX
    		JE P_NOT_CHAR
    	DEC SI
    	MOV DH, [SI]
    	CMP DH, 'a'
    		JB P_NOT_CHAR
    	CMP DH, 'z'
    		JA P_NOT_CHAR
    	MOV DH,1
    		JMP P_DONE
    	P_NOT_CHAR:
    		MOV DH,0
    	P_DONE:
    		POP SI
    	RET
    ENDP   
    ;***********************************************************
    PROC CHECK_NEXT
    	;GETS STRING FROM SI
    	;RETURNS IN DH
    	; IF DH==1 THEN IT'S A CHARACTER
    	PUSH SI
    	MOV DH, [SI]
    	CMP DH, 'a'
    		JB N_NOT_CHAR
    	CMP DH, 'z'
    		JA N_NOT_CHAR
    	MOV DH,1
    		JMP N_DONE
    	N_NOT_CHAR:
    		MOV DH,0
    	N_DONE:
    		POP SI
    	RET
    ENDP
    ;***********************************************************
    TO_LOWER_CASE MACRO BUFFER
    	PUSHA
    	LOCAL L, UPPER_FOUND, NOT_CHAR, FINISH
    	XOR SI,SI
    	L:
    		MOV DL, [BUFFER+SI] ;DL POINTS AT CHARACTER IN THE BUFFER
    		CMP DL, '$' ;IF '$' IS FOUND THEN FINISH
    		JE FINISH
    		CMP DL, 5AH 
    		JBE UPPER_FOUND ; IF IT'S SMALLER THAN OR EQUAL 'Z'
    		INC SI
    		JMP L
    		UPPER_FOUND:
    			CMP DL, 41H ; AND IF IT'S LARGER THAN OR EQUAL 'A'
    			JB NOT_CHAR ;IF NOT THEN IT'S NOT A CHARACTER
    			ADD DL, 20H ; ADD 20H ( THE ASCII OF 'a' )
    			MOV [BUFFER+SI], DL ; MOVE THE VALUE TO THE BUFFER
    			NOT_CHAR:
    				INC SI
    				JMP L
    	FINISH:
    		POPA
    ENDM
    ;***********************************************************  
    CLEAN_PUNC_MARKS MACRO BUFFER, LENGTH
    	PUSHA
    	LOCAL L, L1,L2, FINISH,NEXT_PUNC_MARK,NEXT_CHAR 
    	MOV BX, OFFSET PUNCTUATION_MARKS ; BX = ADDRESS OF FIRST PUNCTUATION MARK
    	XOR AX, AX
    	L2:
    	MOV CX, LENGTH ; CX = LENGTH OF BUFFER		 
    	XOR SI,SI
    	XLAT ;AL = THE VALUE OF THE ADDRESS THAT [BX IS POINTING AT + AL]
    		L:
    			CMP CX,0
    			JBE NEXT_PUNC_MARK ; IF BUFFER IS ALL ITERATED
    			LEA DI, [BUFFER+SI] ;DI STORES THE ADDRESS OF THE CHARACTER
    			MOV DL, [DI] ;DL ITERATES THROUGH THE CHARACTERS OF THE BUFFER
    			
    			CMP AL, DL ; IF CHAR AT BUFFER == PUNCTUATION MARK
    			JNE NEXT_CHAR 
    			;IF THEY'RE EQUAL DELETE PUNCTUATION MARK FROM BUFFER
    			PUSH CX
    			PUSH SI					   
    			
    			MOV SI, DI ; SI = MATCH ADDRESS
    			INC DI ; LET DI POINT TO THE NEXT CHAR
    			MOV CX, LAST_ADDRESS
    			SUB CX, SI
    			ADD CX, 2 ;CX = LAST ADDRESS - MATCH ADDRESS + '$$'
    			XCHG SI,DI
    			REP MOVSB
    			
    			DEC LAST_ADDRESS  
    			
    			POP SI
    			POP CX
    		NEXT_CHAR:
    		INC SI ;POINT TO THE NEXT CHAR IN THE BUFFER
    		DEC CX ;LOOPS--
    		JMP L
    		NEXT_PUNC_MARK:
    			CMP AH, PUNC_MARKS_SIZE
    			JE FINISH
    			INC AH
    			MOV AL, AH
    			JMP L2
    	FINISH:
    	POPA
    ENDM
    ;*********************************************************** 
    CLEAN_DUPLICATE MACRO BUFFER, LENGTH, NO_WORDS
    	PUSHA	   
    	LOCAL NEXT_WORD_IN_BUFFER, L, L2,FINISH, NOT_EQUAL_LENGTH,L_NE,CONTINUE,EQUAL,DEC_LENGTH
    	XOR DI, DI
    	XOR CX, CX
    	MOV AX, NO_WORDS
    	DEC AX ;exclude the last word
    	
    	L:
    		MOV SI, OFFSET BUFFER
    	   
    		L2:
    			MOV DI, SI
    			JMP CONTINUE
    			
    			L_NE:
    			POP CX
    			POP DI
    			POP SI
    			JMP CONTINUE
    			NOT_EQUAL_LENGTH:
    				POP AX		
    				
    			CONTINUE: 
    				COUNT_WORD_LETTERS DI
    				ADD DI, CX
    				INC DI
    				MOV DL, [DI]
    				CMP DL, '$'
    				JE NEXT_WORD_IN_BUFFER
    				
    				PUSH AX
    				MOV AX, CX ;AX now contains the length of the word DI is pointing at
    				COUNT_WORD_LETTERS SI ;CX now contains the length of the word SI is pointing at
    				CMP AX, CX
    				JNE NOT_EQUAL_LENGTH
    				POP AX
    				
    				PUSH SI
    				PUSH DI 
    				PUSH CX
    				;STORE THE STARTING ADDRESS OF THE DUP WORD AND ITS LENGTH		 
    				
    				REPE CMPSB
    				JNE L_NE
    				;IF EQUAL THEN
    				EQUAL:
    				POP CX ;CX = LEGNTH OF WORD
    				POP DI ;DI = START ADDRESS OF DUP WORD
    				POP SI
    				
    				PUSH SI ; SI = ADDRESS(BUFFER) OF THE WORD ( FIRST OCCURANCE ) 
    				PUSH DI
    				PUSH CX		  
    				
    				MOV SI, DI
    				DEC SI
    				ADD DI,CX
    				
    				MOV CX, LAST_ADDRESS
    				ADD CX, 2 ;INCLUDE THE LAST "$$" CHARACTERS
    				SUB CX, DI ;CX = LENGTH OF REMAINING CHARACTERS + "$$" AFTER THE TWO STRINGS MATCH
    				XCHG SI, DI
    				REP MOVSB ;COPY THE CHARACTERS AFTER THE DUP FOUND AND CARRY ON	   
    				
    				POP CX ; CX = LENGTH OF STOP WORD		 
    				POP DI ; DI = START ADDRESS OF STOP WORD
    				POP SI ; SI = ADDRESS OF WORD WHERE THEY'RE EQUAL
    																				  
    				PUSH CX ; STORE THE LENGTH OF STOP WORD BECAUSE IT WILL BE USED IN THE ITERATION
    				DEC_LENGTH:		
    					DEC LAST_ADDRESS ;LAST ADDRESS IS DECREMENTED 
    					DEC LENGTH ;AFTER WE CUT OFF THE DUP WORD FROM THE BUFFER; ITS LENGTH SHRINKED
    					DEC SI ;POINT AT THE CHARACTER THAT REPLACED THE DUP WORD
    					LOOP DEC_LENGTH
    				POP CX ; LOAD THE VALUE BECAUSE IT WILL BE RE-USED AGAIN FOR THE REST OF THE BUFFER
    				
    				JMP CONTINUE
    			
    			NEXT_WORD_IN_BUFFER:
    				DEC AX
    				CMP AX,0
    				JE FINISH
    				COUNT_WORD_LETTERS SI
    				ADD SI, CX
    				INC SI
    				MOV DH, [SI]
    				CMP DH, '$'
    				JE FINISH
    				JMP L2
    				
    		FINISH:	   
    	POPA
    ENDM
    ;***********************************************************	
    GET_LENGTH MACRO BUFFER
    	PUSH BP
    	PUSH DI
    	LOCAL LOOP1, ENDLOOP							 
    	XOR BP, BP
    	LOOP1:
    		CMP BYTE PTR BUFFER ,'$'
    		JE ENDLOOP	  
    		INC BP
    		INC BUFFER
    		JMP LOOP1
    	ENDLOOP:
    		MOV CX, BP
    	POP DI  
    	POP BP
    ENDM
    ;***********************************************************
    COUNT_WORDS MACRO BUFFER, LENGTH, NO_WORDS
    	PUSHA	   
    	LOCAL L, L2
    	MOV CX, LENGTH 
    	XOR SI, SI
    	XOR DI, DI
    	L:  
    		MOV DL, [BUFFER+SI]
    		CMP DL, 20h
    		JNE L2
    		INC DI
    		L2:
    		INC SI
    		LOOP L
    	MOV NO_WORDS, DI
    	INC NO_WORDS
    	POPA
    ENDM
    ;***********************************************************
    COUNT_WORD_LETTERS MACRO BUFFER
    	;BUFFER MUST POINT AT THE START OF THE WORD
    	;RETURNS IN CX
    	;USED FOR REMOVING DUPLICATES
    	PUSH BP
    	PUSH SI
    	PUSH DI
    	PUSH DX
    	LOCAL L, FINISH
    	
    	XOR BP, BP   
    	L:
    		MOV DL, [BUFFER+BP]
    		CMP DL, 20h
    		JE FINISH
    		CMP DL, '$'
    		JE FINISH
    		INC BP
    		JMP L 
    	FINISH:	   
    	MOV CX, BP 
    	
    	POP DX
    	POP DI
    	POP SI
    	POP BP
    ENDM 
    ;*********************************************************** 
    CALC_INTERSECTION PROC NEAR
    	PUSHA
    	MOV AX, NO_WORDS1
    	MOV SI, OFFSET BUFFER1 
    	
    	RESET_DI:  
    		MOV BX, NO_WORDS2 
    		MOV DI, OFFSET BUFFER2
    	COMPARE:
    	
    		COUNT_WORD_LETTERS DI
    		MOV DX, CX
    		COUNT_WORD_LETTERS SI
    		CMP DX, CX
    		JNE NEXT_WORD_IN_DI 
    		
    		REPE CMPSB
    		JNE NEXT_WORD_IN_DI
    		INC INTERSECTION
    	
    	NEXT_WORD_IN_DI:
    		DEC BX
    		CMP BX, 0
    		JE NEXT_WORD_IN_SI
    		COUNT_WORD_LETTERS DI ;returns in CX
    		ADD DI, CX
    		INC DI
            MOV DH, [DI]
            CMP DH, '$'
            JE NEXT_WORD_IN_SI
            JMP COMPARE
            
        NEXT_WORD_IN_SI:
            DEC AX
            CMP AX, 0
            JE FINISH
            COUNT_WORD_LETTERS SI ; returns in CX
            ADD SI, CX
            INC SI
            MOV DL, [SI]
            CMP DL, '$'
            JE FINISH                     
            JMP RESET_DI
            
        FINISH:
            
        POPA
        RET
    ENDP
    ;***********************************************************
    CALC_UNION PROC NEAR
        MOV AX, NO_WORDS1
        ADD AX, NO_WORDS2
    	SUB AX, INTERSECTION
    	MOV UNION, AX
    	RET			  
    ENDP
    
    ;***********************************************************
START:
.STARTUP
    MOV AX,DS
    MOV ES,AX	 
    
    OPEN_FILE FILE_1,  HANDLEF_1
    READ_FROM_FILE HANDLEF_1, 50, BUFFER1, LENGTH1 
    CLOSE_FILE HANDLEF_1 
    
    TO_LOWER_CASE BUFFER1 
       
    CLEAN_STOP_WORDS BUFFER1, LENGTH1
    CLEAN_PUNC_MARKS BUFFER1, LENGTH1	 
    
    COUNT_WORDS BUFFER1,LENGTH1,NO_WORDS1
    
    CLEAN_DUPLICATE BUFFER1, LENGTH1,NO_WORDS1
    
    OPEN_FILE FILE_2,  HANDLEF_2		  
    READ_FROM_FILE HANDLEF_2, 50, BUFFER2, LENGTH2		 
    CLOSE_FILE HANDLEF_2 
    TO_LOWER_CASE BUFFER2 
    CLEAN_STOP_WORDS BUFFER2, LENGTH2 
    CLEAN_PUNC_MARKS BUFFER2, LENGTH2 
    COUNT_WORDS BUFFER2,LENGTH2,NO_WORDS2
    CLEAN_DUPLICATE BUFFER2, LENGTH2,NO_WORDS2 
    
    
    
    COUNT_WORDS BUFFER1,LENGTH1,NO_WORDS1  
    COUNT_WORDS BUFFER2,LENGTH2,NO_WORDS2		 
    
    CALL CALC_INTERSECTION
    CALL CALC_UNION
    
    MOV DX, 0
    MOV AX, 100d
    MUL INTERSECTION
    DIV UNION
    MOV RESULT, AX
    PRINT_MSG SIM_MSG
    CALL PRINT_NUM   
    			   
  
TERMINATE:	  
END