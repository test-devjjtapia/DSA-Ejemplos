
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BINARY-SEARCH-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SORTED-LIST.
          05 NUMBERS OCCURS 10 TIMES PIC 9(2).

       01 TARGET-VALUE PIC 9(3).
       01 FOUND-INDEX  PIC 9(2) VALUE 0.
       01 LOW-INDEX    PIC 9(2).
       01 HIGH-INDEX   PIC 9(2).
       01 MID-INDEX    PIC 9(2).
       01 I            PIC 9(2).

       PROCEDURE DIVISION.

       MOVE  2 TO NUMBERS(1).
       MOVE  5 TO NUMBERS(2).
       MOVE  8 TO NUMBERS(3).
       MOVE 12 TO NUMBERS(4).
       MOVE 16 TO NUMBERS(5).
       MOVE 23 TO NUMBERS(6).
       MOVE 38 TO NUMBERS(7).
       MOVE 56 TO NUMBERS(8).
       MOVE 72 TO NUMBERS(9).
       MOVE 91 TO NUMBERS(10).

       DISPLAY "Lista ordenada: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       DISPLAY "--- BUSQUEDA DE UN ELEMENTO QUE EXISTE ---".
       MOVE 23 TO TARGET-VALUE.
       PERFORM BINARY-SEARCH-ROUTINE.
       IF FOUND-INDEX > 0
           DISPLAY "El elemento " TARGET-VALUE " se encuentra en el indice: " FOUND-INDEX
       ELSE
           DISPLAY "El elemento " TARGET-VALUE " no se encontro en la lista."
       END-IF.

       DISPLAY " ".
       DISPLAY "--- BUSQUEDA DE UN ELEMENTO QUE NO EXISTE ---".
       MOVE 40 TO TARGET-VALUE.
       PERFORM BINARY-SEARCH-ROUTINE.
       IF FOUND-INDEX > 0
           DISPLAY "El elemento " TARGET-VALUE " se encuentra en el indice: " FOUND-INDEX
       ELSE
           DISPLAY "El elemento " TARGET-VALUE " no se encontro en la lista."
       END-IF.

       STOP RUN.

       BINARY-SEARCH-ROUTINE.
           MOVE 1 TO LOW-INDEX.
           MOVE 10 TO HIGH-INDEX.
           MOVE 0 TO FOUND-INDEX.
           PERFORM UNTIL LOW-INDEX > HIGH-INDEX OR FOUND-INDEX > 0
               COMPUTE MID-INDEX = (LOW-INDEX + HIGH-INDEX) / 2
               IF NUMBERS(MID-INDEX) = TARGET-VALUE
                   MOVE MID-INDEX TO FOUND-INDEX
               ELSE
                   IF NUMBERS(MID-INDEX) > TARGET-VALUE
                       SUBTRACT 1 FROM MID-INDEX GIVING HIGH-INDEX
                   ELSE
                       ADD 1 TO MID-INDEX GIVING LOW-INDEX
                   END-IF
               END-IF
           END-PERFORM.
