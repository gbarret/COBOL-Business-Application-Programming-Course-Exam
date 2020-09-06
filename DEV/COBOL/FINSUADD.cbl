       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINSUADD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STATEZIP
              ASSIGN          TO STATEZIP
              ORGANIZATION    IS SEQUENTIAL
              ACCESS MODE     IS SEQUENTIAL
              FILE STATUS     IS WS-STATEZIP-STATUS
           .
       DATA DIVISION.
       FILE SECTION.
       FD  STATEZIP
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 27 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS STATEZIP-REC.
       01  STATEZIP-REC.
           05 STATE-NAME           PIC X(15).
           05 ABBREVIATION         PIC X(02).
           05 ZIP-CODE-LOW         PIC X(05).
           05 ZIP-CODE-HIGH        PIC X(05).
       WORKING-STORAGE SECTION.
       77  WS-STATEZIP-STATUS      PIC X(02) VALUE SPACES.
           88 WS-STATEZIP-STATUS-OK VALUE '00'.
       77  WS-STATEZIP-EOF         PIC X(01) VALUE SPACES.
           88 IS-STATEZIP-EOF      VALUE 'Y'.
       77  WS-ZIP-FOUND            PIC X(01) VALUE SPACES.
           88 WS-ZIP-FOUND-YES     VALUE 'Y'.
           88 WS-ZIP-FOUND-NO      VALUE 'N'.
       77  WS-STATEZIP-REC-READ    PIC 9(03) VALUE ZEROS.
      *
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
      *
       01  STATE-ADDRESS-ZIP-TABLE.
           05 WS-STATE-ID               PIC S9(4) BINARY.
           05 WS-STATE-ADDRESS-ZIP    OCCURS 0 TO 200 TIMES
                                   DEPENDING ON WS-STATE-ID
                                   ASCENDING WS-ABBREVIATION,
                                             WS-ZIP-CODE-LOW
                                   INDEXED BY W-I.
              10 WS-STATE-NAME          PIC X(15).
              10 WS-ABBREVIATION        PIC X(02).
              10 WS-ZIP-CODE-LOW        PIC X(05).
              10 WS-ZIP-CODE-HIGH       PIC X(05).
      *
       LINKAGE SECTION.
       COPY PARTSUPP.
       01  WS-EDIT-RESULTS.
           05 WS-EDIT-STATUS            PIC X(01).
              88 WS-PASS-EDIT-OK        VALUE 'Y'.
           05  WS-EDIT-FIELD            PIC X(20).
           05  WS-EDIT-ERROR-MESSAGE    PIC X(40).

      * ** Edits SUPPLIERS info send from the FINAL01 program
       PROCEDURE DIVISION USING PART-SUPP-ADDR-PO, WS-EDIT-RESULTS.
           PERFORM 000-HOUSEKEEPING

      * ** Resets edit status
           MOVE 'Y'                TO WS-EDIT-STATUS
      * ** Edits
           DISPLAY 'Validating Supp-Address:' PART-NUMBER
           PERFORM 200-EDITS
           CLOSE STATEZIP
           GOBACK   *> Control returned to MAIN
           .
      *
       000-HOUSEKEEPING.
           OPEN INPUT STATEZIP
           PERFORM 100-LOAD-STATE-ADDRESS-TABLE
           .
      *
       100-LOAD-STATE-ADDRESS-TABLE.
           PERFORM UNTIL IS-STATEZIP-EOF
              READ STATEZIP
                 AT END
                    MOVE 'Y'            TO WS-STATEZIP-EOF
                 NOT AT END
                    ADD +1              TO WS-STATEZIP-REC-READ
                    MOVE WS-STATEZIP-REC-READ
                                        TO WS-STATE-ID
                    SET W-I             TO WS-STATE-ID
                    MOVE STATE-NAME     TO WS-STATE-NAME(W-I)
                    MOVE ABBREVIATION   TO WS-ABBREVIATION(W-I)
                    MOVE ZIP-CODE-LOW   TO WS-ZIP-CODE-LOW(W-I)
                    MOVE ZIP-CODE-HIGH  TO WS-ZIP-CODE-HIGH(W-I)
              END-READ
           END-PERFORM
           .
      *
       200-EDITS.
           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL ADDR-IDX > 3
      * **    Checking Required Fields ...
               IF ADDRESS-1(ADDR-IDX) = SPACES
                 MOVE ' ADDRESS-1 IS REQUIRED '
                                        TO WS-EDIT-ERROR-MESSAGE
                 MOVE 'N'               TO WS-EDIT-STATUS
                 EXIT PARAGRAPH
               END-IF
      *
               IF CITY(ADDR-IDX) = SPACES
                  MOVE ' CITY IS REQUIRED'
                                            TO WS-EDIT-ERROR-MESSAGE
                  MOVE 'N'                  TO WS-EDIT-STATUS
                  EXIT PARAGRAPH
               END-IF
      *
               IF ADDR-STATE(ADDR-IDX) = SPACES
                  MOVE ' ADDR-STATE IS REQUIRED'
                                            TO WS-EDIT-ERROR-MESSAGE
                  MOVE 'N'                  TO WS-EDIT-STATUS
                  EXIT PARAGRAPH
               END-IF
      *
               IF ZIP-CODE(ADDR-IDX) IS NOT NUMERIC OR
                  ZIP-CODE(ADDR-IDX) = 0
                  MOVE ' ZIP-CODE IS REQUIRED: '
                                            TO WS-EDIT-ERROR-MESSAGE
                  MOVE 'N'                  TO WS-EDIT-STATUS
                  EXIT PARAGRAPH
               END-IF
      *     ** Checking ADDRESS-TYPE ...
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
      * **     Checking ZIP-CODE & ADDR-STATE against file
               MOVE 'N'                 TO WS-ZIP-FOUND
               PERFORM VARYING W-I FROM 1 BY 1
                 UNTIL W-I > WS-STATE-ID OR WS-ZIP-FOUND-YES
                 IF (ADDR-STATE(ADDR-IDX) = WS-ABBREVIATION(W-I) AND
                    ZIP-CODE(ADDR-IDX)(1:5) >= WS-ZIP-CODE-LOW(W-I) AND
                    ZIP-CODE(ADDR-IDX)(1:5) <= WS-ZIP-CODE-HIGH(W-I))
                    MOVE 'Y'            TO WS-ZIP-FOUND
                 END-IF
               END-PERFORM
               IF (WS-ZIP-FOUND-NO)
                  MOVE ZIP-CODE(ADDR-IDX)(1:5)
                                        TO WS-EDIT-FIELD
                  MOVE ' ZIP-CODE IS not valid.'
                                        TO WS-EDIT-ERROR-MESSAGE
                  MOVE 'N'               TO WS-EDIT-STATUS
                  EXIT PARAGRAPH
               END-IF
           END-PERFORM


           .
      *