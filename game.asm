.MODEL SMALL
.STACK 100h
.DATA  

LARGENUM DW 10000                                 ;NUMBER FOR MOD IN RANDOMINT
A DW 0                                                                                                            
B DW 0                                            ;(A*SEED+B)%LARGENUM=RANDOMINT                                   
SEED DW 0                                         ;USER WILL DEFINE THIS
RANDNUM DW 0                                      ;RANDOMNUM AFTER CALL THE PROC PUT IN THIS
RAND1 DW 0                                        ;RANDOM NUMBER 1
RAND2 DW 0                                        ;RANDOM NUMBER 2
TWO DW 2                                          ;IT USE FOR RANDOM OPERATION
SUBTRACT DW 0                                     ;IF THE NUMBER WAS ODD THIS FLAG BECOME 1
RESULT DW 0                                       ;THE RESULT THAT USER INPUT
NEWLINE DB 10, 13, "$"                            ;USE FOR GO TO NEWLINE
TEN Dw 10D                                        ;USE FOR DIVIDE TO 10
ANSWER DW 0                                       ;THE CORRECT ANSWER
TRUEANS DW 0                                      ;IF THE ANSWER AND RESULT ARE SAME IT BECOME 1 (TRUE)
                                  
MSG1 DB 'PLEASE ENTER YOUR NAME: $'
NAMEUSER DB '$$$$$$$$$$'                          ;NAME OF USER
MSG2 DB 'PLEASE ENTER THE SEED: $'  
score dw 0                                        ;SCORE OF THE USER
SED DB '$$$$'                                     ;GET THE SEED FROM USER AND CONVERT IT TO INTIGER AND PUT IN SEED
ROW DB 2                                          ;ROW OF THE OPERATION PART
COL DB 0                                          ;COLUMN OF THE OPERATION PART
ROWTIME DB 3                                      ;ROW OF THE TIME LINE
COLTIME DB 0                                      ;COLUMN OF THE TIME
NUM DB 6 DUP('$')                                 ;NUMBER THAT USER INPUT AS ANSWER
COUNT   dW      38D                               ;COUNTER FOR TIMELINE
                                  
CHECKER DB 0                                      ;CHECK IF THE FIRST INPUT IS - (FOR NEG NUMBERS)
COLOFINPUTNUM DB 0                                ;COLUMN OF INPUT USER
ROWOFINPUTNUM DB 0                                ;ROW OF INPUT USER
SCOREMSG DB 'SCORE:$' 
RED_COLOR db 0CH                                  ;RED COLOR
NUMBEROFDIGIT DW 0                                ;NUMBER OF DIGIT OF REAL ANSWER
NUMBEROFDIGITUSER DW 0                            ;NUMBER OF DIGIT OF USER ANSWER
 
DIVIDOR DW 10D                                    ;DIVIDE TO 10

COPYANSWER DW 0                                   ;KEEP COPY OF THE ANSWER
TIMEDONEFLAG DW 0                                 ;FLAG TO CHECK TIME IS DONE
NUMBEROFMISTAKE DW 0                              ;NUMBER OF MISTAKES OR MISS
SECOND1 DW 0                                      ;TIME OF START THE OPERATION
SECOND2 DW 0                                      ;TIME OP FINISH THE OPERATIOM
CALCULATETIME DW 0                                ;CALCULATE THE REMAIN TIME
.CODE

MAIN PROC
    MOV AX, @DATA           
    MOV DS, AX 
    MOV AH, 00h                                   ; Set video mode function
    MOV AL, 03h                                   ; Video mode 3h (80x25 text mode)
    INT 10H    
    
    LEA     DX,MSG1                               ;SHOW MS1 FOR GET NAME
    MOV     AH, 9
    INT     21H   
    

    MOV AH, 0Ah                                   ; Function 0Ah of INT 21h reads a string from standard input
    LEA DX, NAMEUSER 
    INT 21h 
    
     
    MOV AH, 9
    LEA DX, newline                               ; Print a new line
    INT 21h 
    
    
    LEA     DX, MSG2
    MOV     AH, 9
    INT     21H   
    

    MOV AH, 0Ah                                   ; Function 0Ah of INT 21h reads a string from standard input to get seed
    LEA DX, SED 
    INT 21h 
    
    
                                                  ;scroll the screen to have a clear screen
    
    MOV AH, 07h                                   ; scroll down function id.
    MOV AL, 2                                     ; lines to scroll.
    MOV BH, 07                                    ; attribute for new lines.
    MOV CL, 0                                     ; upper col.
    MOV CH, 0                                     ; upper row.
    MOV DL, 80                                    ; lower col.
    MOV DH, 1                                     ; lower row.
    INT 10H   
    
    MOV AH, 2                                     ; set curser
    MOV DH, 0                                     ; row.
    MOV DL, 0                                     ; column.
    MOV BH, 0                                     ; page number.
    INT 10H
    
    CALL DISPLAY                                  ;call to display the name of user
      
    MOV AH, 9
    LEA DX, NEWLINE                               ; Print a new line
    INT 21H
    
    MOV AH, 9
    LEA DX, SCOREMSG                              ;display score message
    INT 21h  
    
    MOV DL, ' '
    MOV AH, 02H                                   ;print an space
    INT 21H
    
    MOV DL, ' '
    MOV AH, 02H 
    INT 21H                                       ;print an space
      
    MOV DL, '0'
    MOV AH, 02H                                   ;start score from 0
    INT 21H
    
    MOV AH, 9                                     ; Print a new line
    LEA DX,NEWLINE
    INT 21H
     
                                                  ;call to convert the string seed to integer
    CALL CONVERTSEED 
   
    
    
    
                                                  ;infinite loop for game
    LP:                                           ;check the number of mistake and miss
    CMP NUMBEROFMISTAKE,3                         ;if it is 3, game over
    JE DONE                                       ;check if arrive to the end of the screen
    CMP ROW,24                                    ;if not jump this part
    JNE SCREENISNOTFULL
                                                  ;scroll up 2 lines for new operations
    
    MOV AH, 06H                                   ; scroll up function id.
    MOV AL, 2                                     ; lines to scroll.
    MOV BH, 07                                    ; attribute for new lines.
    MOV CL, 0                                     ; upper col.
    MOV CH, 2                                     ; upper row.
    MOV DL, 80                                    ; lower col.
    MOV DH, 23                                    ; lower row.
    INT 10H  
    
    DEC ROW                                       ;save the place of row because in every iteration it add with 2
    DEC ROW
    MOV AH,02                                     ;set the curser position for new operaion
    MOV BH,00                                     ;set page
    MOV DH,ROW                                    ;set row
    MOV DL,COL                                    ;set column
    INT 10H                                       
    
    DEC ROWTIME                                   ;dec rowtime because it add with 2 in every iteration and when arrive the end of the screen dont need it
    DEC ROWTIME
    
    
    SCREENISNOTFULL:                              ;screen has empty place
    
               
    CALL RANDINT                                  ;call to make a random int between 0 to 9999
    MOV AX,RANDNUM                                ;put it in ax
    MOV RAND1,AX                                  ;put ax in rand1
    CALL PRINTI                                   ;call printi to print the random number
    CALL RANDBIT                                  ;call randbit to define the operation(+,-)
    CMP SUBTRACT, 1                               ;if subtract flag be 1 it should be minus
    JNE POS                                       ;jmp to pos(add)
    MOV DL, '-'                                   ;print minus
    MOV AH, 02H 
    INT 21H                                       
    
    CALL RANDINT                                  ;call randint to get a new random int
    MOV AX,RANDNUM                                ;mov to ax
    MOV RAND2,AX                                  ;mov to rand2
    
    CALL PRINTI                                   ;call printi to print the random int
    MOV AX,RAND1
    SUB AX,RAND2                                  ;minus two numbers
    MOV ANSWER,AX                                 ;put the real answer in ANSWER
    JMP ED                                        ;jmp to ED label

    POS:                                          ;add operation
    MOV DL, '+'                                   ;print plus
    MOV AH, 02H 
    INT 21H                                       
    
    CALL RANDINT                                  ;call randint to make the second randint
    MOV AX,RANDNUM
    MOV RAND2,AX                                  ;save it in rand2
    CALL PRINTI                                   ;print it
    MOV AX,RAND1
    ADD AX,RAND2                                  ;add two random numbers
    MOV ANSWER,AX                                 ;keep it in answer
    ED:
    MOV DL, '='
    MOV AH, 02H                                   ;print =
    INT 21H
    CALL NUMBEROFDIG                              ;call numberofdigit to find number of char of the real answer
                                                  
    MOV AH,03
    MOV BH,00                                     ;get cursor position
    INT 10H
    
    MOV ROW,DH                                   ;save row
    MOV COL,DL                                   ;save column
    MOV ROWOFINPUTNUM,DH                         ;save row of input of user
    MOV COLOFINPUTNUM,DL                         ;save column of input user
    
    MOV SI,OFFSET NUM                            ;set si to offset num
    
    MOV AH,2CH                                   ; To get System Time
    INT 21H
    MOV AX, 0
    MOV AL,DH                                    ; sec 
    MOV SECOND1, AX                              ;save the start time of let user input
    
    
    AGAIN:                                       ;label to get input answer
    CALL TIMEE                                   ;check time is over
    CMP COUNT,0                                  ;jmp to timedone
    JE TIMEDONE
    MOV AH,01                                    ; Function to read a character from standard input without echoing
    INT 16H
    JZ AGAIN                                     ; If the character is zero, meaning Enter key, jump to AGAIN to get another input
    
    
    
    MOV AH,0                                     ; Function to read a character from standard input with echoing
    INT 16H  
    
    CMP AL,13                                    ; Check if the character is Enter (carriage return)
    JE EXIT                                      ; If Enter is pressed, exit the loop    
    
    CMP AL,8                                     ;check if the char is backspace 
    JE DEL                                       ;jmp to edit the number
    
    MOV AH,02                                    ;set the cursor
    MOV BH,00                                    ;page 00
    MOV DH,ROW                                   ;go to row of operation
    MOV DL,COL                                   ;go to the last column that you were                           
    INT 10H
  
 
    MOV AH,02                                    ; Function to output character to standard output
    MOV DL,AL                                    ; Move the character read into DL for printing
    INT 21H
    
    INC COL                                      ;inc column for next input chars
            
      
    MOV [SI],AL                                  ;put the ascii code of input char in the si place
    INC SI                                       ;inc si
    
    JMP AGAIN                                    ;jmp to again

DEL:                                             ;label for edit part
  
   
   DEC COL                                       ;go one column back
   DEC SI                                        ;dec si to edit the number in the array
  
   
                                                                                                                    
   MOV AH,02                                     ;set the cursor to the input place                                        
   MOV BH,00                                     ;page 00
   MOV DH,ROW                                    ;row of input
   MOV DL,COL                                    ;column -1 of input
   INT 10H
   
   MOV AH,06                                     ;scroll one column (char that we want to edit) make it clear
   MOV AL,1                                      ;one line
   MOV BH,07
   MOV CH,ROW                                    ;row of input
   MOV CL,COL                                    ;column of input
   MOV DH,ROW                                    ;row of input
   MOV DL,COL                                    ;column of input
   INT 10H 
   
   MOV [SI],36                                   ;put $ in the place of that char in array
   JMP AGAIN                                     ;jmp to again
   
                                                 ;label of time done
TIMEDONE:                                        ;mov 1 to timedoneflag
   MOV TIMEDONEFLAG,1                            ;jump to convdn label
   JMP CONVDN
                                                 ;exit label
EXIT: 
    MOV AH,2CH                                   ; To get System Time
    INT 21H
    MOV AX, 0
    MOV AL,DH                                    ; sec 
    MOV SECOND2, AX                              ;save the second of end of the operation(after put enter) in second2
    
    mov ax,SECOND2                             ;check to if second2 is bigger than second1
    cmp ax,SECOND1       
    jge  TIMEISOK
    
    mov ax,60d                                   ;if it is not first 60-second 1 and then + second2 to find use time
    sub ax,SECOND1       
    add ax,SECOND2        
    mov CALCULATETIME,ax                         ;put use time in calculatetime
    jmp endl                                     ;jump
    
    
    
TIMEISOK:    
    sub ax,SECOND1                               ;if second2>second1 sub them
    mov CALCULATETIME,ax                         ;put use time in calculatetime
    
ENDL:                                            ;endl label
    MOV AX,20D                                   ;ax=20-calculatetime
    SUB AX,CALCULATETIME                         ;calculatetime=ax
    MOV CALCULATETIME,AX     
                                                 ; Convert the string to a number
    MOV SI, OFFSET NUM  
    MOV AL,[SI] 
    MOV CHECKER,AL                               ;check the first char is - or not (the number is negative or not)
    CMP AL,2DH   
    JE NEGATIVENUMBER                            ;if it is jump
    
POSITIVENUMBER:                                  ;number is positive
CONVERT_LOOP:                                    ;in each step, get the ascii code in array, if it is not $, it is a digit, shift the number(*10)+digit
    MOV AL, [SI]                                 ; Load the ASCII digit   
    CMP AL, 36                          
    JE CONVERT_DONE 
    INC NUMBEROFDIGITUSER
    SUB AL, '0'                                  ; Convert ASCII to binary
    MOV BL, AL                                   ; Move the digit to BL register
    MOV AX, RESULT
    MUL TEN 
    ADD AX, BX                                   ; Add the digit to the result
    MOV RESULT, AX                               ;put the number in result
    INC SI                                       ;inc si
    JMP CONVERT_LOOP                             ;jmp to convert_loop
    
    
NEGATIVENUMBER:                                  ;the number is negative(first char is -)
    INC NUMBEROFDIGITUSER                        
    INC SI                                       ;plus si to dont read -
CONVERT_LOOP2:
    MOV AL, [SI]                                ; Load the ASCII digit   
    CMP AL, 36                          
    JE CONVERT_DONE2  
    INC NUMBEROFDIGITUSER  
    SUB AL, '0'                                  ; Convert ASCII to binary
    MOV BL, AL
    MOV AX, RESULT
    MUL TEN
    ADD AX, BX                                   ; Add the digit to the result
    MOV RESULT, AX                               ; put number in result
    INC SI                                       ; Move to the next digit
    JMP CONVERT_LOOP2                            ;jmp 
    
CONVERT_DONE:                                    ;convert operation for + numbers finished
                                                 
    JMP CONVDN                                   ;jmp cnvdn

     
    
    
    
CONVERT_DONE2:                                   ;convert of - numbers finished

    MOV AX,0                                     ;sub result to make it negative in register
    SUB AX,RESULT
    MOV RESULT,AX
   
    
                                                 ;operations after convert
CONVDN:                                          ;set cursor to the first of where user input a number
    MOV AH, 02h                                  ;page 00
    MOV BH, 0                                    ;row of the input user
    MOV DH, ROWOFINPUTNUM                        ;column of the inout user
    MOV DL, COLOFINPUTNUM                        ; column where the text starts
    INT 10H
    MOV AX,RESULT
    CMP AX,ANSWER                                ;ckeck the answer of the user and real answer are equal
    JE CORRECT                                   ;if it is correct,jmp
    CMP TIMEDONEFLAG,0                           ;else check time was done?
    JE INCORRECT                                 ;if is not so the answer is incorrect, jmp
    
                                                 ;set the color to yellow
    MOV AH, 9                                    
    MOV BL, 0Eh 
    MOV CX, NUMBEROFDIGIT
    INT 10H  
     
    CMP ANSWER,0                                 ;check if the number is negative
    JGE PO                                       ;make the number positive
    NEG ANSWER
    MOV DL, '-'                                  ;if answer is negative,print - at first
    MOV AH, 02H 
    INT 21H
PO:                                              ;the number is positive
    MOV AX,ANSWER
    CALL PRINTI                                  ;call printi to print the number in yellow
    INC NUMBEROFMISTAKE                          ;increase the number of mistake or miss
    JMP DONESTEP                                 ;jump 
    
    
    
CORRECT:                                         ;the number is correct
    MOV AH, 9                                    ;set the color to green
    MOV BL, 0Ah                                                                                                         
    MOV CX, NUMBEROFDIGIT
    INT 10H 
    CMP ANSWER,0                                 ;check if the number is negative
    JGE POSITI
    NEG ANSWER                                   ;make the number positive
    MOV DL, '-'                                  ;if it is print -
    MOV AH, 02H 
    INT 21H  
    POSITI:                                      ;positive number
    MOV AX,ANSWER                                ;call printi to print the asnwer in green
    CALL PRINTI
    MOV AX,CALCULATETIME                         ;add the calculatenumber(remain time) to user's score
    ADD SCORE,AX 
                                                 ;set the cursor to print the new score
    MOV     AH, 2
    MOV     DH, 1                                ; row.
    MOV     DL, 8                                ; column.
    MOV     BH, 0                                ; page number.   
    INT 10H
    MOV AX,SCORE
    CALL PRINTI                                  ;call printi to print the score
 
    JMP DONESTEP                                 ;done
   
    
    
    
INCORRECT:                                       ;incorrect number
    INC NUMBEROFMISTAKE                          ;increase the number of mistake or miss
    MOV AH, 9                                    ;set the color to red for the incorrect answer
    MOV BL, 0Ch 
    MOV CX, NUMBEROFDIGITUSER
    INT 10H  
    CMP RESULT,0                                 ;check if the input answer is - or not
    JGE POSITIVE
    NEG RESULT                                   ;make the result positive
    MOV DL, '-'                                  ;print -
    MOV AH, 02H 
    INT 21H 
POSITIVE:                                        ;positive number
    MOV AX,RESULT
                                                 ;call printi to print the incorrect answer
    CALL PRINTI                                  ;print an space for correct answer
    MOV DL, ' '
    MOV AH, 02H 
    INT 21H                                      ;change the color to yellow
    MOV AH, 9
    MOV BL, 0Eh 
    MOV CX, NUMBEROFDIGIT
    INT 10H                                      ;check if the real answer is negative or nor
    CMP ANSWER,0
    JGE ITSPOSI                                  ;make the real answer positive
    NEG ANSWER
    MOV DL, '-'                                  ;print -
    MOV AH, 02H 
    INT 21H
ITSPOSI:                                         ;positive number
    MOV AX,ANSWER                                ;print the correct answer
    CALL PRINTI
    JMP DONESTEP
    
    
                                                 ;finish one operation,reset all the registers and memory registers
DONESTEP:
    MOV RANDNUM,0 
    MOV RAND1,0
    MOV RAND2,0
    MOV SUBTRACT,0
    MOV RESULT,0 
    MOV ANSWER,0 
    MOV TRUEANS,0  
                               
    INC ROW                                      ;row+2
    INC ROW                                      ;column=0
    MOV COL,0 
    INC ROWTIME                                  ;rowtime+2
    INC ROWTIME
    MOV COLTIME,0                                ;columntime=0
    MOV CX,0 
    MOV SI,OFFSET NUM                            ;reset the input array,put $ in every index
WHIL:
    CMP CX,6
    JE TAMAM
    MOV [SI],36 
    INC SI 
    INC CX
    JMP WHIL

TAMAM:
    MOV count,38D 
     
    MOV CHECKER,0  
    MOV COLOFINPUTNUM,0
    MOV ROWOFINPUTNUM,0 
    MOV NUMBEROFDIGIT,0 
    MOV NUMBEROFDIGITUSER,0    
    MOV DIVIDOR,10D  
    MOV COPYANSWER,0  
    MOV TIMEDONEFLAG,0  
    MOV AH,02                                    ;set the cursor for new operation
    MOV BH,00
    MOV DH,ROW
    MOV DL,COL
    INT 10H
    MOV SECOND1,0
    MOV SECOND2,0
    MOV CALCULATETIME,0 

    JMP LP

DONE:     
                                                 ;exit from the program
    MOV AH, 4CH            
    INT 21H 
    
                        
MAIN ENDP 


DISPLAY proc                                     ;display proc to print the name of the user
    MOV DI,OFFSET NAMEUSER+2
    
    AGAIN2: CMP [DI],13                          ;check the char is enter?
    JE LAST
                                                 ;print the char
    MOV DL,[DI]
    MOV AH,2
    INT 21H
    INC DI
    JMP AGAIN2
    
    
    
    
    LAST: 
    
    RET                                          ;return
    DISPLAY ENDP 

PRINTI PROC
   
    CMP AX,0                                     ;check if the number is 0 , print it and return
    
    JNE NOTZERO
    MOV DL, '0'
    MOV AH, 02H 
    INT 21H
    RET
    
                                                 ;nonzero numbers
    NOTZERO:
                                                 ; Set initial value
    MOV CX, 0
    MOV DX, 0
        
    LABEL1:
                                                 ; Loop condition
    CMP AX, 0
    JE PRINT1
        
                                                 ; Divide the input by 10
    MOV BX, 10
    DIV BX
        
                                                 ; Push remainder to stack
    PUSH DX
        
                                                 ; Loop jump
    INC CX
    XOR DX, DX
    JMP LABEL1
        
    PRINT1:
                                                 ; Loop condition
    CMP CX, 0
    JE EXIT_PRINT
        
                                                 ; Pop the last digit from stack
    POP DX
        
                                                 ; Turn it to ASCII code
    ADD DX, 48
        
                                                 ; Print it
    MOV AH, 02H
    INT 21H
        
                                                 ; Loop jump
    DEC CX
    JMP PRINT1
        
    EXIT_PRINT:
    RET
PRINTI   ENDP
                                                 ; make random int
RANDINT PROC 
    MOV AH,2CH                                   ; To get System Time                             
    INT 21H
    MOV AX, 0
    MOV AL,DH                                    ; sec 
    MOV A, AX                                    ; put the second in A as (A*seed+B)
          
                                                                                                  
    MOV AH,2CH                                   ; To get System Time
    INT 21H
    MOV AX, 0
    MOV AL,CL                                    ; min      
    MOV B, AX                                    ; put the minute in B as (A*seed+B)
    
    MOV AX, SEED
    MUL A                                        ;DX=(A*seed+B)%largenumber
    ADD AX, B
    SUB DX, DX
    DIV LARGENUM                                 ;new seed=dx
    MOV RANDNUM, DX  
    MOV SEED,DX 
    RET
RANDINT ENDP  
                                                 ; make random bit
RANDBIT PROC
    MOV AH,2CH                                   ; To get System Time
    INT 21H
    MOV AX, 0
    MOV AL,DH                                    ;A = sec 
    MOV A, AX 
          
    
    MOV AH,2CH                                   ; To get System Time
    INT 21H
    MOV AX, 0
    MOV AL,CL                                    ;B = min      
    MOV B, AX 
    
    MOV AX, SEED
    MUL A                                        ; DX=(A*seed+B)%2
    ADD AX, B
    SUB DX, DX
    DIV TWO
    CMP DX, 0                                    ; if the number is even , our operator will be +
    JE  ADDI
    
    SUBI:
    MOV SUBTRACT, 1                              ; if the number is odd , oor operator will be -
    RET
    
    ADDI:
    
    RET                                          ;return
RANDBIT ENDP 

CONVERTSEED PROC                                 ; convert seed in an array to integer number
    
    MOV SI, OFFSET SED+2
    
    CONVLOOP:
    MOV AL, [SI]                                 ; Load the ASCII digit   
    CMP AL, 13                                   ; check if it is enter                                
    JE CONVDONE 
    SUB AL, '0'                                  ; Convert ASCII to binary                             
    MOV BL, AL                                   ; Move the digit to BL register                        
    MOV AX, SEED                                                                                        
    MUL TEN
    ADD AX, BX                                   ; Add the digit to the result
    MOV SEED, AX
    INC SI                                       ; Move to the next digit
    JMP CONVLOOP
               
               
    CONVDONE:
    RET                                          ; return
  CONVERTSEED ENDP

TIMEE PROC                                       ;proc to print # and make delay
    
    MOV AH,02                                    ;set cursor to the continoue of the timeline
    MOV BH,00
    MOV DH,ROWTIME
    MOV DL,COLTIME
    INT 10H
    INC COLTIME
    INC COLTIME
 
NEXT_CHAR:                                       ;check if the time is done
    CMP COUNT, 0                                 ;if it is  stop
    JE      STOP

    CMP COUNT ,26                                ;12 # for the first part(green)

    JG  GREEN

    CMP COUNT , 14                               ;12 # for the second part(yellow)

    JG yellow 
    jmp red                                      ;14 #  for the third part(red)


yellow:   
   mov ah, 09h                                   ; Write character and attribute at cursor position
   mov bh, 00h                                   ; Page number (0 for default)
   mov al, '#'                                   ; ASCII character code ('#' in this case)
   mov cx, 2                                     ; Repeat count (how many times to write the character)
   mov bl, 0Eh                                   ; Color attribute (09h = green on black)
   int 10h   
    JMP dn

GREEN:
   mov ah, 09h                                   ; Write character and attribute at cursor position
   mov bh, 00h                                   ; Page number (0 for default)
   mov al, '#'                                   ; ASCII character code ('#' in this case)
   mov cx, 2                                     ; Repeat count (how many times to write the character)
   mov bl, 0ah                                   ; Color attribute (0ah = blue on black)
   int 10h   
   JMP DN 

RED:
   mov ah, 09h                                   ; Write character and attribute at cursor position
   mov bh, 00h                                   ; Page number (0 for default)
   mov al, '#'                                   ; ASCII character code ('#' in this case)
   mov cx, 2                                     ; Repeat count (how many times to write the character)
   mov bl, 0ch                                   ; Color attribute (0ch = red on black)
   int 10h   


   
DN:                                              ;decrease counter
    DEC COUNT


    MOV     CX, 04h                              ; Set the delay time to 4 ticks      
                                                 ; Set the delay time to 0.5 seconds (500000 microseconds)
    MOV     DX, 2000h                            ; Set the delay time to 0.5 seconds (500000 microseconds)
    MOV     AH, 86h                              ; AH register holds the function number for the desired BIOS service (delay)
    INT     15h                                  ; Call the timer interrupt



    JC      STOP   

    RET    

STOP:

      RET

TIMEE ENDP  
                                                 ;calculate the number of char of real answer
NUMBEROFDIG PROC
    MOV AX,ANSWER                                ;put a copy of number in copyanswer
    MOV COPYANSWER,AX
    CMP COPYANSWER,0                             ;if it is positive jump to calculate                                     
    JGE divider_loop 
    INC NUMBEROFDIGIT                            ;if it is negative,increase the numberofdgiti(for - char)
    NEG COPYANSWER 
divider_loop:                                    ;make the copy positive
    MOV AX,COPYANSWER                            ;check if the number is 0 make the numberof digit=1
    CMP AX,0                                     
    JNE DIVIDE_LOOP
    INC NUMBEROFDIGIT
    
DIVIDE_LOOP:
    CMP AX, 0                                    ; Check if the number is 0
    JE  END_LOOP                                 ; If zero, exit the loop
    INC NUMBEROFDIGIT                            ; Increment the counter for each digit  
    SUB DX,DX
    DIV DIVIDOR
    JMP DIVIDE_LOOP                              ; Repeat the loop   
 END_LOOP:
    
    RET
 NUMBEROFDIG ENDP

