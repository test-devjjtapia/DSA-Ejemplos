
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUICK-SORT-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNSORTED-LIST.
          05 NUMBERS OCCURS 7 TIMES PIC 9(2).
       
       01 STACK-AREA.
          05 STACK-LOW  OCCURS 10 TIMES PIC 9(2).
          05 STACK-HIGH OCCURS 10 TIMES PIC 9(2).
          05 STACK-TOP  PIC 9(2) VALUE 0.

       01 I            PIC 9(2).
       01 J            PIC 9(2).
       01 PIVOT        PIC 9(2).
       01 TEMP         PIC 9(2).
       01 LOW-IDX      PIC 9(2).
       01 HIGH-IDX     PIC 9(2).
       01 N            PIC 9(2) VALUE 7.

       PROCEDURE DIVISION.

       MOVE 10 TO NUMBERS(1).
       MOVE 7  TO NUMBERS(2).
       MOVE 8  TO NUMBERS(3).
       MOVE 9  TO NUMBERS(4).
       MOVE 1  TO NUMBERS(5).
       MOVE 5  TO NUMBERS(6).
       MOVE 90 TO NUMBERS(7).

       DISPLAY "Lista desordenada: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       PERFORM QUICK-SORT-ROUTINE.

       DISPLAY "Lista ordenada:    " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       STOP RUN.

       PUSH-STACK.
           ADD 1 TO STACK-TOP.
           MOVE LOW-IDX TO STACK-LOW(STACK-TOP).
           MOVE HIGH-IDX TO STACK-HIGH(STACK-TOP).

       POP-STACK.
           MOVE STACK-LOW(STACK-TOP) TO LOW-IDX.
           MOVE STACK-HIGH(STACK-TOP) TO HIGH-IDX.
           SUBTRACT 1 FROM STACK-TOP.

       PARTITION-ROUTINE.
           MOVE LOW-IDX TO I.
           MOVE HIGH-IDX TO J.
           MOVE NUMBERS(HIGH-IDX) TO PIVOT. *> Using last element as pivot

           PERFORM UNTIL I >= J
               PERFORM UNTIL NUMBERS(I) >= PIVOT OR I >= J
                   ADD 1 TO I
               END-PERFORM
               PERFORM UNTIL NUMBERS(J) <= PIVOT OR J <= I
                   SUBTRACT 1 FROM J
               END-PERFORM
               IF I < J
                   MOVE NUMBERS(I) TO TEMP
                   MOVE NUMBERS(J) TO NUMBERS(I)
                   MOVE TEMP TO NUMBERS(J)
               END-IF
           END-PERFORM.
           MOVE NUMBERS(I) TO TEMP
           MOVE NUMBERS(HIGH-IDX) TO NUMBERS(I)
           MOVE TEMP TO NUMBERS(HIGH-IDX).

       QUICK-SORT-ROUTINE.
           MOVE 1 TO LOW-IDX.
           MOVE N TO HIGH-IDX.
           PERFORM PUSH-STACK.

           PERFORM UNTIL STACK-TOP = 0
               PERFORM POP-STACK
               IF LOW-IDX < HIGH-IDX
                   PERFORM PARTITION-ROUTINE
                   IF I - 1 > LOW-IDX
                       MOVE LOW-IDX TO STACK-LOW(STACK-TOP + 1)
                       MOVE I - 1 TO STACK-HIGH(STACK-TOP + 1)
                       ADD 1 TO STACK-TOP
                   END-IF
                   IF I + 1 < HIGH-IDX
                       MOVE I + 1 TO STACK-LOW(STACK-TOP + 1)
                       MOVE HIGH-IDX TO STACK-HIGH(STACK-TOP + 1)
                       ADD 1 TO STACK-TOP
                   END-IF
               END-IF
           END-PERFORM.
