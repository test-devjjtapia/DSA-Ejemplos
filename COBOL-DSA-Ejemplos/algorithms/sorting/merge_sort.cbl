
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGE-SORT-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNSORTED-LIST.
          05 NUMBERS OCCURS 7 TIMES PIC 9(2).
       
       01 TEMP-ARRAY.
          05 TEMP-NUMBERS OCCURS 7 TIMES PIC 9(2).

       01 I            PIC 9(2).
       01 N            PIC 9(2) VALUE 7.
       01 CURRENT-SIZE PIC 9(2).
       01 LEFT-START   PIC 9(2).

       PROCEDURE DIVISION.

       MOVE 38 TO NUMBERS(1).
       MOVE 27 TO NUMBERS(2).
       MOVE 43 TO NUMBERS(3).
       MOVE 3  TO NUMBERS(4).
       MOVE 9  TO NUMBERS(5).
       MOVE 82 TO NUMBERS(6).
       MOVE 10 TO NUMBERS(7).

       DISPLAY "Lista desordenada: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       PERFORM MERGE-SORT-ROUTINE.

       DISPLAY "Lista ordenada:    " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       STOP RUN.

       MERGE-SORT-ROUTINE.
           MOVE 1 TO CURRENT-SIZE.
           PERFORM UNTIL CURRENT-SIZE >= N
               MOVE 1 TO LEFT-START
               PERFORM UNTIL LEFT-START >= N
                   PERFORM MERGE-PASS
                   COMPUTE LEFT-START = LEFT-START + 2 * CURRENT-SIZE
               END-PERFORM
               COMPUTE CURRENT-SIZE = 2 * CURRENT-SIZE
           END-PERFORM.

       MERGE-PASS.
           COMPUTE MID = LEFT-START + CURRENT-SIZE - 1.
           COMPUTE RIGHT-END = FUNCTION MIN(LEFT-START + 2 * CURRENT-SIZE - 1, N).
           IF MID < RIGHT-END
               PERFORM MERGE-SUB-ARRAYS
           END-IF.

       MERGE-SUB-ARRAYS.
           MOVE LEFT-START TO I.
           COMPUTE J = MID + 1.
           MOVE LEFT-START TO K.
           PERFORM UNTIL I > MID OR J > RIGHT-END
               IF NUMBERS(I) <= NUMBERS(J)
                   MOVE NUMBERS(I) TO TEMP-NUMBERS(K)
                   ADD 1 TO I
               ELSE
                   MOVE NUMBERS(J) TO TEMP-NUMBERS(K)
                   ADD 1 TO J
               END-IF
               ADD 1 TO K
           END-PERFORM.
           PERFORM VARYING L FROM I BY 1 UNTIL L > MID
               MOVE NUMBERS(L) TO TEMP-NUMBERS(K)
               ADD 1 TO K
           END-PERFORM.
           PERFORM VARYING L FROM J BY 1 UNTIL L > RIGHT-END
               MOVE NUMBERS(L) TO TEMP-NUMBERS(K)
               ADD 1 TO K
           END-PERFORM.
           PERFORM VARYING L FROM LEFT-START BY 1 UNTIL L > RIGHT-END
               MOVE TEMP-NUMBERS(L) TO NUMBERS(L)
           END-PERFORM.
