       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINAL01.
      *============================
       ENVIRONMENT DIVISION.
      *============================
       INPUT-OUTPUT SECTION.
      *----------------------------
       FILE-CONTROL.
      *---------------------------
           SELECT PARTSUPP    ASSIGN TO PARTSUPP
             ORGANIZATION     IS  SEQUENTIAL
             ACCESS MODE      IS  SEQUENTIAL
             FILE STATUS      IS  WS-PARTSUPP-STATUS
             .
      *
      *    SELECT CLAIMRPT    ASSIGN TO CLAIMRPT.
      *
      *    SELECT PRTLINE     ASSIGN TO PRTLINE.
       DATA DIVISION.
       FILE SECTION.
       FD  PARTSUPP
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PART-SUPP-ADDR-PO.
       01  PARTSUPP-REC                 PIC X(473).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-PARTSUPP-STATUS        PIC X(02) VALUE '00'.
             88 WS-PARTSUPP-STATUS-OK   VALUE '00'.
      *
       01  FLAGS.
           05 WS-EOF                    PIC X VALUE SPACES.
              88 EOF-FOUND              VALUE 'Y'.
      *
       01  COUNTERS.
           05 WS-PARTSUPP-READ          PIC 9(03) VALUE ZEROS.
      *
       01  WS-SUBROUTINE-NAMES.
           05 WS-FINPARTS               PIC X(12) VALUE 'FINPARTS'.
           05 WS-FINSUPPL               PIC X(12) VALUE 'FINSUPPL'.
           05 WS-FINSUADD               PIC X(12) VALUE 'FINSUADD'.

      * ** Data referenced by subroutines (LINKAGE SECTION)
       COPY PARTSUPP.
       01  WS-EDIT-RESULTS.
           05 WS-EDIT-STATUS            PIC X(01).
              88 WS-PASS-EDIT-OK        VALUE 'Y'.
           05  WS-EDIT-FIELD            PIC X(20).
           05  WS-EDIT-ERROR-MESSAGE    PIC X(40).
      *
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING
           PERFORM UNTIL EOF-FOUND
              PERFORM 100-EDITS
              PERFORM 200-READ-NEXT
           END-PERFORM
      *
           CLOSE PARTSUPP
           GOBACK
           .
      *
       000-HOUSEKEEPING.
           OPEN INPUT PARTSUPP
           IF NOT WS-PARTSUPP-STATUS-OK
              DISPLAY ' Error opening file PARTSUPP, Status: '
                         WS-PARTSUPP-STATUS
              GO TO 900-ABEND
           END-IF

           PERFORM 200-READ-NEXT
           .
      *
       100-EDITS.
           MOVE 'Y'                TO WS-EDIT-STATUS
           MOVE SPACES             to WS-EDIT-FIELD
           CALL WS-FINPARTS USING PART-SUPP-ADDR-PO, WS-EDIT-RESULTS
           IF NOT WS-PASS-EDIT-OK
             DISPLAY ' - Parts Edit: FAILED '
                ', Error: ' WS-EDIT-ERROR-MESSAGE
             DISPLAY ' ** Invalid Value: ' WS-EDIT-FIELD
             EXIT PARAGRAPH   *> No more validations - no more calls
           ELSE
             DISPLAY ' - Parts Edit: PASSED'
           END-IF
      *
           MOVE 'Y'                TO WS-EDIT-STATUS
           CALL WS-FINSUPPL USING PART-SUPP-ADDR-PO, WS-EDIT-RESULTS
           IF NOT WS-PASS-EDIT-OK
             DISPLAY ' - Supplies Edit: FAILED '
                ', Error: ' WS-EDIT-ERROR-MESSAGE
             DISPLAY ' ** Invalid Value: ' WS-EDIT-FIELD
             EXIT PARAGRAPH   *> No more validations - no more calls
           ELSE
             DISPLAY ' - Supplies Edit: PASSED'
           END-IF
      *
           MOVE 'Y'                TO WS-EDIT-STATUS
           CALL WS-FINSUADD USING PART-SUPP-ADDR-PO, WS-EDIT-RESULTS
           IF NOT WS-PASS-EDIT-OK
             DISPLAY ' - Supp-Address Edit: FAILED '
                ', Error: ' WS-EDIT-ERROR-MESSAGE
             DISPLAY ' ** Invalid Value: ' WS-EDIT-FIELD
             EXIT PARAGRAPH   *> No more validations - no more calls
           ELSE
              DISPLAY ' - Supp-Address Edit: PASSED'
           END-IF
           .
      *
       200-READ-NEXT.
           READ PARTSUPP INTO PART-SUPP-ADDR-PO
             AT END
                 MOVE 'Y'       TO WS-EOF
             NOT AT END
                 ADD +1         TO WS-PARTSUPP-READ
           END-READ
           .
      *
       900-ABEND.
           DISPLAY '*** Error Abnormally end of Program'
           GOBACK
           .