       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINPARTS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       COPY PARTSUPP.
       01  WS-EDIT-RESULTS.
           05 WS-EDIT-STATUS            PIC X(01).
              88 WS-PASS-EDIT-OK        VALUE 'Y'.
           05  WS-EDIT-FIELD            PIC x(20).
           05  WS-EDIT-ERROR-MESSAGE    PIC X(40).

      * ** Edits PARTS info send from the FINAL01 program
       PROCEDURE DIVISION USING PART-SUPP-ADDR-PO, WS-EDIT-RESULTS.
      * ** Resets edit status
           MOVE 'Y'                TO WS-EDIT-STATUS
      * ** Edits
      *
           DISPLAY 'Validating Parts:' PART-NUMBER
           PERFORM 000-EDITS
           GOBACK   *> Control returned to MAIN
           .
      *
       000-EDITS.
      * ** Checking Required Fields ...
           IF PART-NUMBER = SPACES
              MOVE ' PART-NUMBER IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF PART-NAME = SPACES
              MOVE ' PART-NAME IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF VEHICLE-MAKE = SPACES
              MOVE ' VEHICLE-MAKE IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF VEHICLE-MODEL = SPACES
              MOVE ' VEHICLE-MODEL IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF VEHICLE-YEAR IS NOT NUMERIC OR VEHICLE-YEAR = 0
              MOVE ' VEHICLE-YEAR IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking  VEHICLE-MAKE - valid values ..
           IF NOT (CHRYSLER OR FORD OR GM OR VOLKSWAGON OR
              TOYOTA OR JAGUAR OR PEUGEOT OR BMW)
              MOVE VEHICLE-MAKE         TO WS-EDIT-FIELD
              MOVE ' VEHICLE-MAKE IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking  VEHICLE-YEAR - valid values ..
           IF NOT VEHICLE-YEAR-OK
              MOVE VEHICLE-YEAR         TO WS-EDIT-FIELD
              MOVE ' VEHICLE-YEAR IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking  WEEKS-LEAD-TIME - valid values ..
           IF NOT WEEKS-LEAD-TIME-OK
              MOVE WEEKS-LEAD-TIME      TO WS-EDIT-FIELD
              MOVE ' WEEKS-LEAD-TIME IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           .
      *