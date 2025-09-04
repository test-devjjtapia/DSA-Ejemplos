
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACK-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STACK-STRUCTURE.
          05 STACK-ARRAY OCCURS 10 TIMES.
             10 STACK-ITEM PIC X(10).
          05 TOP-POINTER PIC 9(2) VALUE 0.

       01 ITEM-TO-PUSH PIC X(10).
       01 POPPED-ITEM  PIC X(10).
       01 PEEKED-ITEM  PIC X(10).

       PROCEDURE DIVISION.

       DISPLAY "--- INICIALIZANDO PILA ---".
       DISPLAY "¿La pila esta vacia? " (IF TOP-POINTER = 0 THEN "Si" ELSE "No").

       DISPLAY " ".
       DISPLAY "--- AÑADIENDO ELEMENTOS (PUSH) ---".
       MOVE "Libro 1" TO ITEM-TO-PUSH.
       PERFORM PUSH-ROUTINE.
       MOVE "Libro 2" TO ITEM-TO-PUSH.
       PERFORM PUSH-ROUTINE.
       MOVE "Libro 3" TO ITEM-TO-PUSH.
       PERFORM PUSH-ROUTINE.

       DISPLAY "Pila actual: ".
       PERFORM DISPLAY-STACK.
       PERFORM PEEK-ROUTINE.
       DISPLAY "Elemento en la cima (peek): " PEEKED-ITEM.
       DISPLAY "Tamaño de la pila: " TOP-POINTER.

       DISPLAY " ".
       DISPLAY "--- ELIMINANDO ELEMENTOS (POP) ---".
       PERFORM POP-ROUTINE.
       DISPLAY "Elemento quitado: " POPPED-ITEM.
       DISPLAY "Pila actual: ".
       PERFORM DISPLAY-STACK.

       PERFORM POP-ROUTINE.
       DISPLAY "Elemento quitado: " POPPED-ITEM.
       DISPLAY "Pila actual: ".
       PERFORM DISPLAY-STACK.

       PERFORM PEEK-ROUTINE.
       DISPLAY "Elemento en la cima ahora: " PEEKED-ITEM.
       DISPLAY "¿La pila esta vacia? " (IF TOP-POINTER = 0 THEN "Si" ELSE "No").

       STOP RUN.

       PUSH-ROUTINE.
           IF TOP-POINTER < 10
               ADD 1 TO TOP-POINTER
               MOVE ITEM-TO-PUSH TO STACK-ITEM(TOP-POINTER)
           ELSE
               DISPLAY "Error: La pila esta llena."
           END-IF.

       POP-ROUTINE.
           IF TOP-POINTER > 0
               MOVE STACK-ITEM(TOP-POINTER) TO POPPED-ITEM
               SUBTRACT 1 FROM TOP-POINTER
           ELSE
               DISPLAY "Error: La pila esta vacia."
           END-IF.

       PEEK-ROUTINE.
           IF TOP-POINTER > 0
               MOVE STACK-ITEM(TOP-POINTER) TO PEEKED-ITEM
           ELSE
               MOVE SPACES TO PEEKED-ITEM
           END-IF.

       DISPLAY-STACK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOP-POINTER
               DISPLAY FUNCTION TRIM(STACK-ITEM(I))
           END-PERFORM.
