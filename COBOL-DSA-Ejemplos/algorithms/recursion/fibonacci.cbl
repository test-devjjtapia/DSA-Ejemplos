
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N-VALUE PIC 9(2) VALUE 10.
       01 FIB-RESULT PIC 9(10).

       01 ITERATIVE-VARS.
          05 A PIC 9(10) VALUE 0.
          05 B PIC 9(10) VALUE 1.
          05 I PIC 9(2).

       01 RECURSIVE-STACK.
          05 STACK-N OCCURS 20 TIMES PIC 9(2).
          05 STACK-RETURN-ADDR OCCURS 20 TIMES PIC 9(2).
          05 STACK-RESULT OCCURS 20 TIMES PIC 9(10).
          05 STACK-TOP PIC 9(2) VALUE 0.

       01 CURRENT-N PIC 9(2).
       01 CALL-TYPE PIC 9(1).
           88 CALL-LEFT VALUE 1.
           88 CALL-RIGHT VALUE 2.

       PROCEDURE DIVISION.

       DISPLAY "Calculando el Fibonacci de " N-VALUE " con diferentes metodos:".

       DISPLAY " ".
       DISPLAY "--- ENFOQUE ITERATIVO ---".
       PERFORM FIB-ITERATIVE.
       DISPLAY "Iterativo: " FIB-RESULT.

       DISPLAY " ".
       DISPLAY "--- ENFOQUE RECURSIVO (SIMULADO) ---".
       PERFORM FIB-RECURSIVE.
       DISPLAY "Recursivo: " FIB-RESULT.

       STOP RUN.

       FIB-ITERATIVE.
           IF N-VALUE <= 1
               MOVE N-VALUE TO FIB-RESULT
               EXIT PARAGRAPH
           END-IF.
           MOVE 0 TO A.
           MOVE 1 TO B.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > N-VALUE
               COMPUTE FIB-RESULT = A + B
               MOVE B TO A
               MOVE FIB-RESULT TO B
           END-PERFORM.

       FIB-RECURSIVE.
           MOVE 0 TO STACK-TOP.
           MOVE N-VALUE TO CURRENT-N.
           PERFORM PUSH-STACK.

           PERFORM UNTIL STACK-TOP = 0
               MOVE STACK-N(STACK-TOP) TO CURRENT-N
               IF CURRENT-N <= 1
                   MOVE CURRENT-N TO STACK-RESULT(STACK-TOP)
                   PERFORM POP-STACK
               ELSE
                   IF NOT CALL-LEFT AND NOT CALL-RIGHT
                       MOVE 1 TO CALL-TYPE
                       PERFORM PUSH-STACK
                       SUBTRACT 1 FROM CURRENT-N
                       PERFORM PUSH-STACK
                   ELSE IF CALL-LEFT
                       MOVE 2 TO CALL-TYPE
                       PERFORM PUSH-STACK
                       SUBTRACT 2 FROM CURRENT-N
                       PERFORM PUSH-STACK
                   ELSE IF CALL-RIGHT
                       COMPUTE FIB-RESULT = STACK-RESULT(STACK-TOP - 1) + STACK-RESULT(STACK-TOP)
                       SUBTRACT 2 FROM STACK-TOP
                       MOVE FIB-RESULT TO STACK-RESULT(STACK-TOP)
                       PERFORM POP-STACK
                   END-IF
               END-IF
           END-PERFORM.

       PUSH-STACK.
           ADD 1 TO STACK-TOP.
           MOVE CURRENT-N TO STACK-N(STACK-TOP).
           MOVE CALL-TYPE TO STACK-RETURN-ADDR(STACK-TOP).

       POP-STACK.
           SUBTRACT 1 FROM STACK-TOP.
           IF STACK-TOP > 0
               MOVE STACK-RETURN-ADDR(STACK-TOP) TO CALL-TYPE
           ELSE
               MOVE 0 TO CALL-TYPE
           END-IF.
