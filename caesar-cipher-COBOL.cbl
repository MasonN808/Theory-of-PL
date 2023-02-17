        IDENTIFICATION DIVISION.
        PROGRAM-ID. CIPHER.
        
        DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 INPUT-STR PIC X(20).
            01 ENCRYPT-STR PIC X(20).
            01 DECRYPTED-STR PIC X(20).
            01 INPUT-STR-1 PIC X(15) VALUE 'E.T. Phone Home'.
            01 INPUT-STR-2 PIC X(20) VALUE 'mY HoUsE Is On fIrez'.
            
            01 CHAR-ENC PIC X(1).
            01 i PIC 9(3).
            01 j PIC 9(3) VALUE 0.
            01 k PIC 9(3) VALUE 0.
            01 ASCII-VALUE PIC 999.
            01 ASCII-VALUE-T PIC 999.
            01 CHAR-VAL PIC X(1).
            01 SHIFT-VALUE PIC 999.
            01 INPUT-STR-LEN PIC 99 VALUE 0.
            01 REMAINDER-MOD PIC 999 VALUE 0.
            01 DUMMY PIC 999 VALUE 0.
            01 SOLVE-STR PIC X(3).
        
        PROCEDURE DIVISION.
        Begin.
            *> Initial structure pulled from: 
            *> http://www.csis.ul.ie/cobol/examples/SubProg/Multiply/DriverProg.htm
            *> Change data values before calling encrypt
            MOVE FUNCTION UPPER-CASE(INPUT-STR-1) to INPUT-STR-1
            MOVE INPUT-STR-1 TO INPUT-STR
            *> Get the length of the string and store it
            INSPECT INPUT-STR-1 TALLYING INPUT-STR-LEN FOR CHARACTERS.
            *> Assign the shift values
            MOVE 8 TO SHIFT-VALUE
            DISPLAY "ORIGINAL STRING: " INPUT-STR-1
            PERFORM CallEncrypt
            DISPLAY  "ENCRYPTED STRING: " ENCRYPT-STR
            DISPLAY "--- Decrypting ---------"
            PERFORM CallDecrypt
            DISPLAY  "DECRYPTED STRING: ", DECRYPTED-STR
            
            DISPLAY SPACE
            DISPLAY "--------------------------------------"
            *> Reset the pointers for next set of encrypting and decrypting
            MOVE SPACE TO DECRYPTED-STR
            MOVE SPACE TO ENCRYPT-STR
            MOVE 0 TO INPUT-STR-LEN
            MOVE FUNCTION UPPER-CASE(INPUT-STR-2) to INPUT-STR-2
            MOVE INPUT-STR-2 TO INPUT-STR
            *> Get the length of the string and store it
            INSPECT INPUT-STR-2 TALLYING INPUT-STR-LEN FOR CHARACTERS.
            DISPLAY "ORIGINAL STRING: ", INPUT-STR-2
            PERFORM CallEncrypt
            DISPLAY  "ENCRYPTED STRING: ", ENCRYPT-STR
            DISPLAY "--- Decrypting ---------"
            PERFORM CallDecrypt
            DISPLAY  "DECRYPTED STRING: ", DECRYPTED-STR
            
            
            *> Reset the pointers for next set of encrypting and decrypting
            MOVE SPACE TO DECRYPTED-STR
            MOVE 'hal' TO ENCRYPT-STR
            MOVE FUNCTION UPPER-CASE(ENCRYPT-STR) to ENCRYPT-STR
            MOVE 3 TO INPUT-STR-LEN
            DISPLAY "--- Solving ---------"
            PERFORM CallSolve
            
            STOP RUN.
        
        CallEncrypt.
            DISPLAY "--- Encrypting ---------"
            PERFORM varying i from 1 BY 1 UNTIL i>INPUT-STR-LEN
                ADD 1 TO j
                MOVE FUNCTION ORD(INPUT-STR(i:j)) TO ASCII-VALUE
                MOVE FUNCTION ORD(INPUT-STR(i:j)) TO ASCII-VALUE-T
                ADD SHIFT-VALUE TO ASCII-VALUE-T
                *> Look for spaces and periods and skip them
                IF (ASCII-VALUE = 47 OR ASCII-VALUE = 33) THEN
                    MOVE FUNCTION CHAR(ASCII-VALUE) TO CHAR-VAL
                    *> Concatenate the character to the string
                    MOVE CHAR-VAL TO ENCRYPT-STR(i:j)
                ELSE
                    IF (ASCII-VALUE-T >= 66 AND ASCII-VALUE-T <= 91) 
                    THEN
                        MOVE FUNCTION CHAR(ASCII-VALUE-T) TO CHAR-VAL
                        *> Concatenate the character to the string
                        MOVE CHAR-VAL TO ENCRYPT-STR(i:j)
                    ELSE
                        IF (ASCII-VALUE-T > 90) THEN
                            *> This is a mod function
                            DIVIDE ASCII-VALUE-T BY 92
                                GIVING DUMMY
                                REMAINDER REMAINDER-MOD
                            ADD 66 TO REMAINDER-MOD
                            MOVE FUNCTION CHAR(REMAINDER-MOD) 
                            TO CHAR-VAL
                            *> Concatenate the character to the string
                            MOVE CHAR-VAL TO ENCRYPT-STR(i:j)
                    END-IF
                END-IF
            MOVE 0 TO j
            END-PERFORM.
            
        CallDecrypt.
            PERFORM varying i from 1 BY 1 UNTIL i>INPUT-STR-LEN
                ADD 1 TO j
                MOVE FUNCTION ORD(ENCRYPT-STR(i:j)) TO ASCII-VALUE
                MOVE FUNCTION ORD(ENCRYPT-STR(i:j)) TO ASCII-VALUE-T
                SUBTRACT SHIFT-VALUE FROM ASCII-VALUE-T
                *> Look for spaces and periods and skip them
                IF (ASCII-VALUE = 47 OR ASCII-VALUE = 33) THEN
                    MOVE FUNCTION CHAR(ASCII-VALUE) TO CHAR-VAL
                    *> Concatenate the character to the string
                    MOVE CHAR-VAL TO DECRYPTED-STR(i:j)
                ELSE
                    IF (ASCII-VALUE-T >= 66 AND ASCII-VALUE-T <= 91) 
                    THEN
                        MOVE FUNCTION CHAR(ASCII-VALUE-T) TO CHAR-VAL
                        *> Concatenate the character to the string
                        MOVE CHAR-VAL TO DECRYPTED-STR(i:j)
                    ELSE
                        IF (ASCII-VALUE-T <= 65) THEN
                            *> This is a mod function
                            DIVIDE ASCII-VALUE-T BY 66
                                GIVING DUMMY
                                REMAINDER REMAINDER-MOD
                            SUBTRACT 66 FROM REMAINDER-MOD
                            SUBTRACT 92 FROM REMAINDER-MOD
                            *> DISPLAY REMAINDER-MOD
                            MOVE FUNCTION CHAR(REMAINDER-MOD) 
                            TO CHAR-VAL
                            *> Concatenate the character to the string
                            MOVE CHAR-VAL TO DECRYPTED-STR(i:j)
                    END-IF
                END-IF
            MOVE 0 TO j
            END-PERFORM.
            
        CallSolve.
            PERFORM varying k from 0 BY 1 UNTIL k>26
                *> Assign the shift values
                MOVE k TO SHIFT-VALUE
                PERFORM CallDecrypt
                DISPLAY DECRYPTED-STR
            END-PERFORM.



