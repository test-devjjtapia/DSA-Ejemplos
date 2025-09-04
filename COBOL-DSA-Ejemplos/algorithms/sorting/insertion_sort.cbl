
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERTION-SORT-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UNSORTED-LIST.
          05 NUMBERS OCCURS 5 TIMES PIC 9(2).
       
       01 I            PIC 9(2).
       01 J            PIC 9(2).
       01 KEY-ELEMENT  PIC 9(2).
       01 N            PIC 9(2) VALUE 5.

       PROCEDURE DIVISION.

       MOVE 12 TO NUMBERS(1).
       MOVE 11 TO NUMBERS(2).
       MOVE 13 TO NUMBERS(3).
       MOVE 5  TO NUMBERS(4).
       MOVE 6  TO NUMBERS(5).

       DISPLAY "Lista desordenada: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       PERFORM INSERTION-SORT-ROUTINE.

       DISPLAY "Lista ordenada:    " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       STOP RUN.

       INSERTION-SORT-ROUTINE.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > N
               MOVE NUMBERS(I) TO KEY-ELEMENT
               MOVE I TO J
               SUBTRACT 1 FROM J
               PERFORM UNTIL J < 1 OR NUMBERS(J) <= KEY-ELEMENT
                   MOVE NUMBERS(J) TO NUMBERS(J + 1)
                   SUBTRACT 1 FROM J
               END-PERFORM
               MOVE KEY-ELEMENT TO NUMBERS(J + 1)
           END-PERFORM.
