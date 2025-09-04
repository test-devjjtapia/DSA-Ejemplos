
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECTION-SORT-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNSORTED-LIST.
          05 NUMBERS OCCURS 5 TIMES PIC 9(2).
       
       01 I            PIC 9(2).
       01 J            PIC 9(2).
       01 MIN-INDEX    PIC 9(2).
       01 TEMP         PIC 9(2).
       01 N            PIC 9(2) VALUE 5.

       PROCEDURE DIVISION.

       MOVE 64 TO NUMBERS(1).
       MOVE 25 TO NUMBERS(2).
       MOVE 12 TO NUMBERS(3).
       MOVE 22 TO NUMBERS(4).
       MOVE 11 TO NUMBERS(5).

       DISPLAY "Lista desordenada: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       PERFORM SELECTION-SORT-ROUTINE.

       DISPLAY "Lista ordenada:    " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       STOP RUN.

       SELECTION-SORT-ROUTINE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N - 1
               MOVE I TO MIN-INDEX
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > N
                   IF NUMBERS(J) < NUMBERS(MIN-INDEX)
                       MOVE J TO MIN-INDEX
                   END-IF
               END-PERFORM
               MOVE NUMBERS(I) TO TEMP
               MOVE NUMBERS(MIN-INDEX) TO NUMBERS(I)
               MOVE TEMP TO NUMBERS(MIN-INDEX)
           END-PERFORM.
