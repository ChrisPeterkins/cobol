       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEEDLOAD.
      *================================================================*
      * SEEDLOAD - Seed Data Loader                                    *
      * Populates ACCOUNTS.dat with test accounts for demonstration.   *
      *================================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE
               ASSIGN TO "data/ACCOUNTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY ACCT-REC.

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS        PIC XX.
           88  WS-ACCT-OK            VALUE "00".
           88  WS-ACCT-DUP-KEY       VALUE "22".
           88  WS-ACCT-FILE-MISSING  VALUE "35".

       01  WS-COUNT               PIC 9(3) VALUE 0.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURR-YEAR      PIC 9(4).
           05  WS-CURR-MONTH     PIC 9(2).
           05  WS-CURR-DAY       PIC 9(2).
           05  WS-CURR-REST      PIC X(7).

       01  WS-TODAY               PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "========================================="
           DISPLAY "  SEED DATA LOADER"
           DISPLAY "========================================="

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-TODAY
           END-STRING

      *    Create the file fresh
           OPEN OUTPUT ACCT-FILE
           IF NOT WS-ACCT-OK
               DISPLAY "Error creating account file: "
                   WS-ACCT-STATUS
               STOP RUN
           END-IF

      *    Account 1: Alice - Checking with $5,000
           MOVE 10000001        TO ACCT-NO
           MOVE "Alice Johnson"  TO ACCT-NAME
           MOVE 5000.00         TO ACCT-BAL
           MOVE "C"             TO ACCT-TYPE
           MOVE "A"             TO ACCT-STATUS
           MOVE 20250101        TO ACCT-OPEN-DT
           WRITE ACCT-REC
           IF WS-ACCT-OK ADD 1 TO WS-COUNT END-IF

      *    Account 2: Bob - Savings with $12,500
           MOVE 10000002        TO ACCT-NO
           MOVE "Bob Smith"      TO ACCT-NAME
           MOVE 12500.00        TO ACCT-BAL
           MOVE "S"             TO ACCT-TYPE
           MOVE "A"             TO ACCT-STATUS
           MOVE 20250115        TO ACCT-OPEN-DT
           WRITE ACCT-REC
           IF WS-ACCT-OK ADD 1 TO WS-COUNT END-IF

      *    Account 3: Carol - Checking with $850
           MOVE 10000003        TO ACCT-NO
           MOVE "Carol Williams"  TO ACCT-NAME
           MOVE 850.00          TO ACCT-BAL
           MOVE "C"             TO ACCT-TYPE
           MOVE "A"             TO ACCT-STATUS
           MOVE 20250201        TO ACCT-OPEN-DT
           WRITE ACCT-REC
           IF WS-ACCT-OK ADD 1 TO WS-COUNT END-IF

      *    Account 4: Dave - Savings with $25,000
           MOVE 10000004        TO ACCT-NO
           MOVE "David Chen"     TO ACCT-NAME
           MOVE 25000.00        TO ACCT-BAL
           MOVE "S"             TO ACCT-TYPE
           MOVE "A"             TO ACCT-STATUS
           MOVE 20240601        TO ACCT-OPEN-DT
           WRITE ACCT-REC
           IF WS-ACCT-OK ADD 1 TO WS-COUNT END-IF

      *    Account 5: Eve - Closed checking
           MOVE 10000005        TO ACCT-NO
           MOVE "Eve Martinez"   TO ACCT-NAME
           MOVE 0.00            TO ACCT-BAL
           MOVE "C"             TO ACCT-TYPE
           MOVE "X"             TO ACCT-STATUS
           MOVE 20240301        TO ACCT-OPEN-DT
           WRITE ACCT-REC
           IF WS-ACCT-OK ADD 1 TO WS-COUNT END-IF

           CLOSE ACCT-FILE

           DISPLAY SPACES
           DISPLAY "Loaded " WS-COUNT " seed accounts:"
           DISPLAY "  10000001  Alice Johnson    $5,000.00  "
               "Checking  Active"
           DISPLAY "  10000002  Bob Smith       $12,500.00  "
               "Savings   Active"
           DISPLAY "  10000003  Carol Williams     $850.00  "
               "Checking  Active"
           DISPLAY "  10000004  David Chen      $25,000.00  "
               "Savings   Active"
           DISPLAY "  10000005  Eve Martinez         $0.00  "
               "Checking  Closed"
           DISPLAY SPACES
           DISPLAY "Seed data loaded successfully!"

           STOP RUN.
