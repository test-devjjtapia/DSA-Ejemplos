
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINEAR-SEARCH-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MY-LIST.
          05 NUMBERS OCCURS 8 TIMES PIC 9(2).
       
       01 TARGET-VALUE PIC 9(3).
       01 FOUND-INDEX  PIC 9(2) VALUE 0.
       01 I            PIC 9(2).

       PROCEDURE DIVISION.

       MOVE 10 TO NUMBERS(1).
       MOVE 50 TO NUMBERS(2).
       MOVE 30 TO NUMBERS(3).
       MOVE 70 TO NUMBERS(4).
       MOVE 80 TO NUMBERS(5).
       MOVE 20 TO NUMBERS(6).
       MOVE 90 TO NUMBERS(7).
       MOVE 40 TO NUMBERS(8).

       DISPLAY "Lista: " 
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
           DISPLAY NUMBERS(I) WITH NO ADVANCING " "
       END-PERFORM.
       DISPLAY " ".

       DISPLAY "--- BUSQUEDA DE UN ELEMENTO QUE EXISTE ---".
       MOVE 80 TO TARGET-VALUE.
       PERFORM LINEAR-SEARCH-ROUTINE.
       IF FOUND-INDEX > 0
           DISPLAY "El elemento " TARGET-VALUE " se encuentra en el indice: " FOUND-INDEX
       ELSE
           DISPLAY "El elemento " TARGET-VALUE " no se encontro en la lista."
       END-IF.

       DISPLAY " ".
       DISPLAY "--- BUSQUEDA DE UN ELEMENTO QUE NO EXISTE ---".
       MOVE 100 TO TARGET-VALUE.
       PERFORM LINEAR-SEARCH-ROUTINE.
       IF FOUND-INDEX > 0
           DISPLAY "El elemento " TARGET-VALUE " se encuentra en el indice: " FOUND-INDEX
       ELSE
           DISPLAY "El elemento " TARGET-VALUE " no se encontro en la lista."
       END-IF.

       STOP RUN.

       LINEAR-SEARCH-ROUTINE.
           MOVE 0 TO FOUND-INDEX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               IF NUMBERS(I) = TARGET-VALUE
                   MOVE I TO FOUND-INDEX
                   EXIT PERFORM
               END-IF
           END-PERFORM.
