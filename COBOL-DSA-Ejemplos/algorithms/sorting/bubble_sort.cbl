
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUBBLE-SORT-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNSORTED-LIST.
          05 NUMBERS OCCURS 7 TIMES PIC 9(2).
       
       01 I            PIC 9(2).
       01 J            PIC 9(2).
       01 TEMP         PIC 9(2).
       01 N            PIC 9(2) VALUE 7.

       PROCEDURE DIVISION.

       MOVE 64 TO NUMBERS(1).
       MOVE 34 TO NUMBERS(2).
       MOVE 25 TO NUMBERS(3).
       MOVE 12 TO NUMBERS(4).
       MOVE 22 TO NUMBERS(5).
       MOVE 11 TO NUMBERS(6).
       MOVE 90 TO NUMBERS(7).

       DISPLAY "Lista desordenada: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       PERFORM BUBBLE-SORT-ROUTINE.

       DISPLAY "Lista ordenada:    " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       STOP RUN.

       BUBBLE-SORT-ROUTINE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N - 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N - I
                   IF NUMBERS(J) > NUMBERS(J + 1)
                       MOVE NUMBERS(J) TO TEMP
                       MOVE NUMBERS(J + 1) TO NUMBERS(J)
                       MOVE TEMP TO NUMBERS(J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.
