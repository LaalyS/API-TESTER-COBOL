       IDENTIFICATION DIVISION.
       FUNCTION-ID. VERIFY-INTEGER-ADV.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       01 LS-WRKING-VAR.
           05 LS-INPUT-NUMBER-LENGTH    PIC 9(09).
           05 LS-COUNTER             PIC 9(02) VALUE 1.
           05 LS-PERIOD-COUNTER PIC 9(02).
           05 LS-COMMA-COUNTER PIC 9(02).
           05 LS-BOOL-VAR.
               10 LS-PERIODS   PIC 9.
                   88 LS-CONTAINS-PERIODS   VALUE 1.
               10 LS-COMMAS    PIC 9.
                   88 LS-CONTAINS-COMMAS    VALUE 1.
               10 LS-NON-NUMERIC   PIC 9.
                   88 LS-CONTAINS-NON-NUM    VALUE 1.
           05 LS-VECTOR.
               10 LS-EACH-CHARCT   OCCURS 39 TIMES PIC X.
      *
       LINKAGE SECTION.
           01 LN-NUMBER            PIC X ANY LENGTH.
           01 LN-RESULT            PIC 9.
      *
       PROCEDURE DIVISION
       USING     LN-NUMBER
       RETURNING LN-RESULT.
      *
       MAIN-PAR.
      *
           MOVE LENGTH (TRIM(LN-NUMBER))
           TO LS-INPUT-NUMBER-LENGTH
      *
           IF LS-INPUT-NUMBER-LENGTH > 39 THEN
      *
               MOVE    3 TO LN-RESULT
               EXIT PARAGRAPH
      *
           END-IF
      *
           MOVE TRIM (LN-NUMBER) TO LS-VECTOR
      *
           IF LS-EACH-CHARCT (1) = "-" OR "+" THEN
      *
               MOVE 2 TO LS-COUNTER
      *
           END-IF
      *
           PERFORM
           WITH TEST AFTER
           VARYING LS-COUNTER FROM LS-COUNTER BY 1
           UNTIL   LS-COUNTER = 39
      *
               EVALUATE LS-EACH-CHARCT (LS-COUNTER)
      *
               WHEN SPACES
      *
                   PERFORM CHECKING-PAR
      *
               WHEN "0" THROUGH "9"
      *
                   CONTINUE
      *
               WHEN "."
      *
                   SET LS-CONTAINS-PERIODS TO TRUE
                   ADD 1 TO LS-PERIOD-COUNTER
      *
               WHEN ","
      *
                   SET LS-CONTAINS-COMMAS TO TRUE
                   ADD 1 TO LS-COMMA-COUNTER
      *
               WHEN OTHER
      *
                   SET LS-CONTAINS-NON-NUM TO TRUE
                   CONTINUE
      *
           END-PERFORM
      *
           PERFORM CHECKING-PAR.
      *
       CHECKING-PAR.
      *
           IF LS-CONTAINS-NON-NUM THEN
      *
               MOVE 2 TO LN-RESULT
      *
           ELSE IF LS-CONTAINS-PERIODS AND NOT LS-CONTAINS-COMMAS
           THEN
      *
               IF LS-PERIOD-COUNTER = 1 THEN
      *
                   MOVE 4 TO LN-RESULT
      *
               ELSE
      *
                   MOVE 5 TO LN-RESULT
      *
               END-IF
      *
           ELSE IF NOT LS-CONTAINS-PERIODS AND LS-CONTAINS-COMMAS
           THEN
      *
               IF LS-COMMA-COUNTER = 1 THEN
      *
                   MOVE 6 TO LN-RESULT
      *
               ELSE
      *
                   MOVE 7 TO LN-RESULT
      *
               END-IF
      *
           ELSE IF LS-CONTAINS-PERIODS AND LS-CONTAINS-COMMAS
           THEN
      *
               IF  LS-PERIOD-COUNTER = 1
               AND LS-COMMA-COUNTER = 1
               THEN
      *
                   MOVE 8 TO LN-RESULT
      *
               ELSE
      *
                   MOVE 9 TO LN-RESULT
      *
               END-IF
      *
           ELSE
      *
               MOVE 1 TO LN-RESULT
      *
           END-IF.
      *
       END FUNCTION VERIFY-INTEGER-ADV.

       * B"H.




# API-TESTER-COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBAS2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION VERIFY-INTEGER
           FUNCTION VERIFY-INTEGER-ADV
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER  PIC X(58).
       01 WS-NUMBER2 PIC X(58).
       01 WS-RESULT  PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "INPUT INTEGER NUMBER. " WITH NO ADVANCING
           END-DISPLAY
           ACCEPT WS-NUMBER
           DISPLAY SPACES
           
           MOVE VERIFY-INTEGER (WS-NUMBER) TO WS-RESULT
           
           IF WS-RESULT = 1 THEN

               DISPLAY "YOU HAVE INPUT AN INTEGER NUMBER!"
               
           END-IF

           IF WS-RESULT = 3 THEN

               DISPLAY "YOUR INPUT HAS MORE THAN 38 DIGITS!"
               
           END-IF
           
           IF VERIFY-INTEGER (WS-NUMBER) = 0

               DISPLAY "YOU HAVE NOT INPUT AN INTEGER NUMBER."

           END-IF
           
           DISPLAY SPACES
           
           DISPLAY "INPUT INTEGER NUMBER. " WITH NO ADVANCING
           END-DISPLAY
           ACCEPT WS-NUMBER2
           DISPLAY SPACES
           
           MOVE VERIFY-INTEGER-ADV (WS-NUMBER2) TO WS-RESULT
           
           EVALUATE WS-RESULT
           
               WHEN 0
               
                   DISPLAY "ERROR! YOU HAVE INPUT A BLANK AMOUNT!"
                   
               WHEN 1
               
                   DISPLAY "YOU HAVE INPUT AN INTEGER NUMBER!"
                   
               WHEN 2
               
                   DISPLAY "YOUR INPUT CONBTAINS NON-NUMERIC "
                   "CHARACTERS!"
               
               WHEN 3
               
                   DISPLAY "YOUR INPUT HAS MORE THAN 38 DIGITS!"
               
               WHEN 4
               
                   DISPLAY "YOUR INPUT HAS ONE DECIMAL POINT!"
               
               WHEN 5
               
                   DISPLAY "YOUR INPUT HAS MORE THAN ONE DECIMAL"
                   " POINTS!"
               
               WHEN 6
               
                   DISPLAY "YOUR INPUT HAS ONE DECIMAL COMMA!"
               
               WHEN 7
               
                   DISPLAY "YOUR INPUT HAS MORE THAN ONE DECIMAL COMMA!"
               
               WHEN 8
               
                   DISPLAY "YOUR INPUT HAS BOTH ONE DECIMAL PERIOD AND"
                   " ONE DECIMAL COMMA!"
               
               WHEN 9
               
                   DISPLAY "YOUR INPUT HAS MULTIPLE DECIMAL PERIODS AND"
                   " MULTIPLE DECIMAL COMMAS!"
               
               WHEN OTHER
               
                   DISPLAY "I REALLY HAVE NO IDEA OF WHAT'S GOING ON!"
           
           END-EVALUATE
           
           DISPLAY SPACES
           
           DISPLAY "LET'S PLAY SOME REALLY NICE VIDEO GAMES!"

           STOP RUN RETURNING 2.
           
       END PROGRAM PRUEBAS2.
