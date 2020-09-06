       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINSUPPL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  LILIAN                  PIC S9(9) BINARY.
       01  MSG-NO-X                PIC X(10).
      *
       01  IN-DATE.
           02  VSTRING-LENGTH      PIC S9(4) BINARY.
           02  VSTRING-TEXT.
               03  VSTRING-CHAR    PIC X
                           OCCURS 0 TO 256 TIMES
                           DEPENDING ON VSTRING-LENGTH
                               OF IN-DATE.
      *
       01  PICSTR.
           02  VSTRING-LENGTH      PIC S9(4) BINARY.
           02  VSTRING-TEXT.
               03  VSTRING-CHAR    PIC X
                           OCCURS 0 TO 256 TIMES
                           DEPENDING ON VSTRING-LENGTH
                              OF PICSTR.
       01  FC.
           02  Condition-Token-Value.
           COPY  CEEIGZCT.
               03  CASE-1-CONDITION-ID.
                   04  SEVERITY         PIC S9(4) BINARY.
                   04  MSG-NO           PIC S9(4) BINARY.
               03  CASE-2-CONDITION-ID
                         REDEFINES CASE-1-CONDITION-ID.
                   04  CLASS-CODE       PIC S9(4) BINARY.
                   04  CAUSE-CODE       PIC S9(4) BINARY.
               03  CASE-SEV-CTL         PIC X.
               03  FACILITY-ID          PIC XXX.
           02  I-S-INFO                 PIC S9(9) BINARY.
       LINKAGE SECTION.
       COPY PARTSUPP.
       01  WS-EDIT-RESULTS.
           05 WS-EDIT-STATUS            PIC X(01).
              88 WS-PASS-EDIT-OK        VALUE 'Y'.
           05  WS-EDIT-FIELD            PIC X(20).
           05  WS-EDIT-ERROR-MESSAGE    PIC X(40).

      * ** Edits SUPPLIERS info send from the FINAL01 program
       PROCEDURE DIVISION USING PART-SUPP-ADDR-PO, WS-EDIT-RESULTS.
      * ** Resets edit status
           MOVE 'Y'                TO WS-EDIT-STATUS
      * ** Edits
      *
           DISPLAY 'Validating Suppliers:' PART-NUMBER
           PERFORM 000-EDITS
           GOBACK   *> Control returned to MAIN
           .
      *
       000-EDITS.
      * ** Checking Required Fields ...
           IF SUPPLIER-CODE = SPACES
              MOVE ' SUPPLIER-CODE IS REQUIRED '
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF SUPPLIER-TYPE = SPACES
              MOVE ' SUPPLIER-TYPE IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF SUPPLIER-NAME = SPACES
              MOVE ' SUPPLIER-NAME IS REQUIRED'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      *
           IF SUPPLIER-PERF IS NOT NUMERIC OR SUPPLIER-PERF = 0
              MOVE ' SUPPLIER-PERF IS REQUIRED: '
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking  SUPPLIER-TYPE - valid values ..
           IF NOT (SUBCONTRACTOR OR DISTRIBUTOR OR
              MANUFACTURER OR IMPORTER)
              MOVE ' SUPPLIER-TYPE IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking  SUPPLIER-RATING - valid values ..
           IF NOT (HIGHEST-QUALITY OR AVERAGE-QUALITY OR LOWEST-QUALITY)
              MOVE ' SUPPLIER-RATING IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking  SUPPLIER-STATUS - valid values ..
           IF NOT (GOVT-COMM OR GOVT-ONLY OR COMMERCIAL-ONLY)
              MOVE SUPPLIER-STATUS      TO WS-EDIT-FIELD
              MOVE ' SUPPLIER-STATUS IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
              MOVE 'N'                  TO WS-EDIT-STATUS
              EXIT PARAGRAPH
           END-IF
      * ** Checking SUPPLIER-ACT-DATE  using CEEDAYS
      *************************************************
      ** CALL CEEDAYS TO CONVERT THE RETIREMENT DATE **
      ** TO  LILIAN REPRESENTATION                   **
      *************************************************
           DISPLAY ' *DATE TO VALIDATE: ' SUPPLIER-ACT-DATE
           MOVE 8                       TO VSTRING-LENGTH OF IN-DATE
           MOVE SUPPLIER-ACT-DATE       TO
                           VSTRING-TEXT OF IN-DATE(1:8)
           MOVE 8                       TO VSTRING-LENGTH OF PICSTR
      *    MOVE 'YY/MM/DD'              TO VSTRING-TEXT OF PICSTR(1:8)
           MOVE 'YYYYMMDD'              TO VSTRING-TEXT OF PICSTR(1:8)
           CALL 'CEEDAYS' USING IN-DATE, PICSTR,
                                LILIAN, FC
      *************************************************
      ** IF CEEDAYS RUNS SUCCESSFULLY, DISPLAY RESULT**
      *************************************************
           IF  CEE000 OF FC  THEN
               DISPLAY VSTRING-TEXT OF IN-DATE
                   ' IS LILIAN DAY: ' LILIAN
           ELSE
               MOVE SUPPLIER-ACT-DATE   TO WS-EDIT-FIELD
               MOVE MSG-NO OF FC        TO MSG-NO-X
               STRING 'CEEDAYS FAILED WITH MSG ' DELIMITED BY SIZE
                       MSG-NO-X DELIMITED BY SIZE
                       INTO WS-EDIT-ERROR-MESSAGE
               END-STRING
               MOVE 'N'                 TO WS-EDIT-STATUS
               EXIT PARAGRAPH
           END-IF
      * ** Checking SUPP-ADDRESS
           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL ADDR-IDX > 3
              IF NOT (ORDER-ADDRESS(ADDR-IDX) OR
                      SCHED-ADDRESS(ADDR-IDX) OR
                      REMIT-ADDRESS(ADDR-IDX) )
                 MOVE ADDRESS-TYPE(ADDR-IDX)
                                        TO WS-EDIT-FIELD
                 MOVE ' SUPPLIER-STATUS IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
                 MOVE 'N'               TO WS-EDIT-STATUS
                 EXIT PARAGRAPH
              END-IF
           END-PERFORM

           .
      *