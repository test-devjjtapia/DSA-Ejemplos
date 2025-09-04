
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUEUE-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 QUEUE-STRUCTURE.
          05 QUEUE-ARRAY OCCURS 10 TIMES.
             10 QUEUE-ITEM PIC X(10).
          05 FRONT-POINTER PIC 9(2) VALUE 1.
          05 REAR-POINTER  PIC 9(2) VALUE 0.
          05 QUEUE-SIZE    PIC 9(2) VALUE 0.

       01 ITEM-TO-ENQUEUE PIC X(10).
       01 DEQUEUED-ITEM   PIC X(10).
       01 PEEKED-ITEM     PIC X(10).

       PROCEDURE DIVISION.
       
       DISPLAY "--- INICIALIZANDO COLA ---".
       DISPLAY "¿La cola esta vacia? " (IF QUEUE-SIZE = 0 THEN "Si" ELSE "No").

       DISPLAY " ".
       DISPLAY "--- AÑADIENDO ELEMENTOS (ENQUEUE) ---".
       MOVE "Cliente A" TO ITEM-TO-ENQUEUE.
       PERFORM ENQUEUE-ROUTINE.
       MOVE "Cliente B" TO ITEM-TO-ENQUEUE.
       PERFORM ENQUEUE-ROUTINE.
       MOVE "Cliente C" TO ITEM-TO-ENQUEUE.
       PERFORM ENQUEUE-ROUTINE.

       DISPLAY "Cola actual: ".
       PERFORM DISPLAY-QUEUE.
       PERFORM PEEK-ROUTINE.
       DISPLAY "Elemento en el frente (peek): " PEEKED-ITEM.
       DISPLAY "Tamaño de la cola: " QUEUE-SIZE.

       DISPLAY " ".
       DISPLAY "--- ATENDIENDO ELEMENTOS (DEQUEUE) ---".
       PERFORM DEQUEUE-ROUTINE.
       DISPLAY "Cliente atendido: " DEQUEUED-ITEM.
       DISPLAY "Cola actual: ".
       PERFORM DISPLAY-QUEUE.

       PERFORM DEQUEUE-ROUTINE.
       DISPLAY "Cliente atendido: " DEQUEUED-ITEM.
       DISPLAY "Cola actual: ".
       PERFORM DISPLAY-QUEUE.

       PERFORM PEEK-ROUTINE.
       DISPLAY "Proximo cliente a atender: " PEEKED-ITEM.
       DISPLAY "¿La cola esta vacia? " (IF QUEUE-SIZE = 0 THEN "Si" ELSE "No").

       STOP RUN.

       ENQUEUE-ROUTINE.
           IF QUEUE-SIZE < 10
               ADD 1 TO REAR-POINTER
               MOVE ITEM-TO-ENQUEUE TO QUEUE-ITEM(REAR-POINTER)
               ADD 1 TO QUEUE-SIZE
           ELSE
               DISPLAY "Error: La cola esta llena."
           END-IF.

       DEQUEUE-ROUTINE.
           IF QUEUE-SIZE > 0
               MOVE QUEUE-ITEM(FRONT-POINTER) TO DEQUEUED-ITEM
               ADD 1 TO FRONT-POINTER
               SUBTRACT 1 FROM QUEUE-SIZE
           ELSE
               DISPLAY "Error: La cola esta vacia."
           END-IF.

       PEEK-ROUTINE.
           IF QUEUE-SIZE > 0
               MOVE QUEUE-ITEM(FRONT-POINTER) TO PEEKED-ITEM
           ELSE
               MOVE SPACES TO PEEKED-ITEM
           END-IF.

       DISPLAY-QUEUE.
           PERFORM VARYING I FROM FRONT-POINTER BY 1 UNTIL I > REAR-POINTER
               DISPLAY FUNCTION TRIM(QUEUE-ITEM(I))
           END-PERFORM.
