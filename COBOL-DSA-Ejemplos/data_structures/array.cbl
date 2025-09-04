
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAY-DEMO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FRUTAS-ARRAY.
          05 FRUTAS OCCURS 10 TIMES INDEXED BY I.
             10 FRUTA PIC X(10).
       01 NUMEROS-ARRAY.
          05 NUMEROS OCCURS 10 TIMES INDEXED BY J.
             10 NUMERO PIC 9(2).

       01 ARRAY-SIZE PIC 9(2) VALUE 4.
       01 TEMP-FRUTA PIC X(10).
       01 TEMP-NUMERO PIC 9(2).
       01 SUB-ARRAY-SIZE PIC 9(2).

       PROCEDURE DIVISION.
       
       DISPLAY "--- CREACION Y ACCESO ---".
       MOVE "manzana"  TO FRUTA(1).
       MOVE "banana"   TO FRUTA(2).
       MOVE "cereza"   TO FRUTA(3).
       MOVE "datil"    TO FRUTA(4).
       
       DISPLAY "Lista completa: ".
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
           DISPLAY FUNCTION TRIM(FRUTA(I))
       END-PERFORM.

       DISPLAY "Primer elemento (indice 1): " FRUTA(1).
       DISPLAY "Tercer elemento (indice 3): " FRUTA(3).

       DISPLAY " ".
       DISPLAY "--- MODIFICACION ---".
       MOVE "arandano" TO FRUTA(2).
       DISPLAY "Lista despues de modificar el indice 2: ".
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
           DISPLAY FUNCTION TRIM(FRUTA(I))
       END-PERFORM.

       DISPLAY " ".
       DISPLAY "--- AGREGAR ELEMENTOS ---".
       ADD 1 TO ARRAY-SIZE.
       MOVE "frambuesa" TO FRUTA(ARRAY-SIZE).
       DISPLAY "Despues de agregar 'frambuesa': ".
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
           DISPLAY FUNCTION TRIM(FRUTA(I))
       END-PERFORM.

       DISPLAY " ".
       DISPLAY "--- SLICING (REBANADO) ---".
       PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
           MOVE J TO NUMERO(J)
       END-PERFORM.
       
       DISPLAY "Lista de numeros: ".
       PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
           DISPLAY NUMERO(J)
       END-PERFORM.

       DISPLAY "Sub-lista de [3:6]: ".
       PERFORM VARYING J FROM 3 BY 1 UNTIL J > 6
           DISPLAY NUMERO(J)
       END-PERFORM.

       STOP RUN.
