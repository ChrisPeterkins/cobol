       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTGEN.
      *================================================================*
      * RPTGEN - Report Generator                                      *
      * Produces formatted account statements for a given account      *
      * and date range. Demonstrates COBOL report formatting.          *
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

           SELECT TXN-FILE
               ASSIGN TO "data/TRANSACTIONS.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TXN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY ACCT-REC.

       FD  TXN-FILE.
       COPY TXNL-REC.

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS         PIC XX.
           88  WS-ACCT-OK             VALUE "00".
           88  WS-ACCT-NOT-FOUND      VALUE "23".
           88  WS-ACCT-FILE-MISSING   VALUE "35".

       01  WS-TXN-STATUS          PIC XX.
           88  WS-TXN-OK              VALUE "00".
           88  WS-TXN-EOF             VALUE "10".
           88  WS-TXN-FILE-MISSING    VALUE "35".

      *--- Input parameters ---
       01  WS-INPUT-ACCTNO        PIC X(8).
       01  WS-TARGET-ACCTNO       PIC 9(8).
       01  WS-INPUT-START-DATE    PIC X(8).
       01  WS-INPUT-END-DATE      PIC X(8).
       01  WS-START-DATE          PIC 9(8).
       01  WS-END-DATE            PIC 9(8).

      *--- Running balance ---
       01  WS-RUNNING-BAL         PIC S9(9)V99 VALUE ZEROS.

      *--- Report accumulators ---
       01  WS-RPT-DEPOSIT-TOTAL   PIC S9(9)V99 VALUE ZEROS.
       01  WS-RPT-WITHDRAW-TOTAL  PIC S9(9)V99 VALUE ZEROS.
       01  WS-RPT-TRANSFER-TOTAL  PIC S9(9)V99 VALUE ZEROS.
       01  WS-RPT-TXN-COUNT       PIC 9(5) VALUE ZEROS.

      *--- Display fields with PIC editing ---
       01  WS-DISP-BAL            PIC $$$,$$$,$$9.99-.
       01  WS-DISP-AMT            PIC $$$,$$$,$$9.99-.
       01  WS-DISP-DEPOSIT        PIC $$$,$$$,$$9.99.
       01  WS-DISP-WITHDRAW       PIC $$$,$$$,$$9.99.
       01  WS-DISP-ZERO           PIC $$$,$$$,$$9.99 VALUE ZEROS.

      *--- Formatted date fields ---
       01  WS-FMT-DATE.
           05  WS-FMT-YEAR        PIC 9(4).
           05  WS-FMT-SEP1        PIC X VALUE "-".
           05  WS-FMT-MONTH       PIC 9(2).
           05  WS-FMT-SEP2        PIC X VALUE "-".
           05  WS-FMT-DAY         PIC 9(2).

       01  WS-FMT-TIME.
           05  WS-FMT-HH          PIC 9(2).
           05  WS-FMT-TSEP1       PIC X VALUE ":".
           05  WS-FMT-MM          PIC 9(2).
           05  WS-FMT-TSEP2       PIC X VALUE ":".
           05  WS-FMT-SS          PIC 9(2).

       01  WS-RAW-DATE            PIC 9(8).
       01  WS-RAW-TIME            PIC 9(6).

      *--- Account type display ---
       01  WS-TYPE-NAME           PIC X(10).

      *--- Line counter for page breaks ---
       01  WS-LINE-COUNT          PIC 99 VALUE 99.
       01  WS-PAGE-NUM            PIC 999 VALUE 0.
       01  WS-LINES-PER-PAGE      PIC 99 VALUE 50.

      *--- Report line buffer ---
       01  WS-REPORT-LINE         PIC X(80).
       01  WS-SEPARATOR-LINE      PIC X(78) VALUE ALL "-".

      *--- Current date for report header ---
       01  WS-CURRENT-DATE-DATA.
           05  WS-NOW-YEAR        PIC 9(4).
           05  WS-NOW-MONTH       PIC 9(2).
           05  WS-NOW-DAY         PIC 9(2).
           05  WS-NOW-REST        PIC X(13).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM GET-REPORT-PARAMS
           PERFORM GENERATE-REPORT
           STOP RUN.

       GET-REPORT-PARAMS.
           DISPLAY "========================================="
           DISPLAY "  ACCOUNT STATEMENT GENERATOR"
           DISPLAY "========================================="
           DISPLAY "Account number (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCTNO
           MOVE WS-INPUT-ACCTNO TO WS-TARGET-ACCTNO

           DISPLAY "Start date (YYYYMMDD, or Enter for all):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-START-DATE
           IF WS-INPUT-START-DATE = SPACES
               MOVE 00000000 TO WS-START-DATE
           ELSE
               MOVE WS-INPUT-START-DATE TO WS-START-DATE
           END-IF

           DISPLAY "End date (YYYYMMDD, or Enter for today):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-END-DATE
           IF WS-INPUT-END-DATE = SPACES
               MOVE 99999999 TO WS-END-DATE
           ELSE
               MOVE WS-INPUT-END-DATE TO WS-END-DATE
           END-IF.

       GENERATE-REPORT.
      *    Look up the account
           OPEN INPUT ACCT-FILE
           IF NOT WS-ACCT-OK
               DISPLAY "Error opening account file: "
                   WS-ACCT-STATUS
               STOP RUN
           END-IF

           MOVE WS-TARGET-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Account " WS-TARGET-ACCTNO
                       " not found."
                   CLOSE ACCT-FILE
                   STOP RUN
           END-READ

           PERFORM PRINT-REPORT-HEADER

      *    Now scan transactions
           OPEN INPUT TXN-FILE
           IF NOT WS-TXN-OK
               IF WS-TXN-FILE-MISSING
                   DISPLAY "No transaction file found."
               ELSE
                   DISPLAY "Error opening transaction file: "
                       WS-TXN-STATUS
               END-IF
               CLOSE ACCT-FILE
               STOP RUN
           END-IF

      *    We need to figure out starting balance.
      *    Starting balance = current balance - net of all txns
      *    in range. We'll do two passes: first count net, then
      *    display. Or simpler: assume beginning balance is
      *    current balance minus all-time net (show running bal).
      *    For simplicity, compute starting balance by reading
      *    all transactions before start date.

           MOVE ZEROS TO WS-RUNNING-BAL
      *    First pass: compute balance at start of period
           PERFORM CALC-STARTING-BALANCE

      *    Second pass: print transactions in range
           CLOSE TXN-FILE
           OPEN INPUT TXN-FILE
           IF NOT WS-TXN-OK
               CLOSE ACCT-FILE
               STOP RUN
           END-IF

           MOVE WS-RUNNING-BAL TO WS-DISP-BAL
           DISPLAY "  Opening Balance:"
               "                              "
               WS-DISP-BAL
           DISPLAY "  " WS-SEPARATOR-LINE

           PERFORM PRINT-TXN-LINES UNTIL WS-TXN-EOF

           CLOSE TXN-FILE
           CLOSE ACCT-FILE

           PERFORM PRINT-REPORT-FOOTER.

       CALC-STARTING-BALANCE.
      *    Read all transactions for this account before the
      *    start date to build up the opening balance.
           PERFORM READ-FOR-STARTING-BAL UNTIL WS-TXN-EOF.

       READ-FOR-STARTING-BAL.
           READ TXN-FILE
               AT END
                   CONTINUE
               NOT AT END
                   IF TXN-ACCT-NO = WS-TARGET-ACCTNO
                       AND TXN-STATUS = "C"
                       AND TXN-DATE < WS-START-DATE
                       EVALUATE TXN-TYPE
                           WHEN "D"
                               ADD TXN-AMOUNT TO WS-RUNNING-BAL
                           WHEN "W"
                               SUBTRACT TXN-AMOUNT
                                   FROM WS-RUNNING-BAL
                           WHEN "T"
                               SUBTRACT TXN-AMOUNT
                                   FROM WS-RUNNING-BAL
                       END-EVALUATE
                   END-IF
      *            Also handle incoming transfers
                   IF TXN-XFER-ACCT = WS-TARGET-ACCTNO
                       AND TXN-STATUS = "C"
                       AND TXN-TYPE = "T"
                       AND TXN-DATE < WS-START-DATE
                       ADD TXN-AMOUNT TO WS-RUNNING-BAL
                   END-IF
           END-READ.

       PRINT-REPORT-HEADER.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA

           DISPLAY SPACES
           DISPLAY "=================================="
               "=================================="
               "=========="
           DISPLAY "  COBOL BANK LEDGER"
               "                    ACCOUNT STATEMENT"
           DISPLAY "=================================="
               "=================================="
               "=========="
           DISPLAY SPACES
           DISPLAY "  Account:    " ACCT-NO
           DISPLAY "  Name:       " ACCT-NAME

           EVALUATE ACCT-TYPE
               WHEN "C" MOVE "Checking" TO WS-TYPE-NAME
               WHEN "S" MOVE "Savings"  TO WS-TYPE-NAME
               WHEN OTHER MOVE "Unknown" TO WS-TYPE-NAME
           END-EVALUATE
           DISPLAY "  Type:       " WS-TYPE-NAME

           EVALUATE ACCT-STATUS
               WHEN "A" DISPLAY "  Status:     Active"
               WHEN "X" DISPLAY "  Status:     CLOSED"
           END-EVALUATE

           DISPLAY "  Period:     " WS-START-DATE
               " to " WS-END-DATE
           MOVE ACCT-BAL TO WS-DISP-BAL
           DISPLAY "  Cur Balance:" WS-DISP-BAL
           DISPLAY SPACES
           DISPLAY "  Date       Time     Type"
               "        Amount          Balance"
               "         Description"
           DISPLAY "  " WS-SEPARATOR-LINE.

       PRINT-TXN-LINES.
           READ TXN-FILE
               AT END
                   CONTINUE
               NOT AT END
      *            Check if this txn is for our account
                   IF TXN-ACCT-NO = WS-TARGET-ACCTNO
                       AND TXN-STATUS = "C"
                       PERFORM PROCESS-OWN-TXN
                   END-IF
      *            Check for incoming transfers
                   IF TXN-XFER-ACCT = WS-TARGET-ACCTNO
                       AND TXN-STATUS = "C"
                       AND TXN-TYPE = "T"
                       PERFORM PROCESS-INCOMING-XFER
                   END-IF
           END-READ.

       PROCESS-OWN-TXN.
           IF TXN-DATE >= WS-START-DATE
               AND TXN-DATE <= WS-END-DATE
               EVALUATE TXN-TYPE
                   WHEN "D"
                       ADD TXN-AMOUNT TO WS-RUNNING-BAL
                       ADD TXN-AMOUNT TO WS-RPT-DEPOSIT-TOTAL
                   WHEN "W"
                       SUBTRACT TXN-AMOUNT FROM WS-RUNNING-BAL
                       ADD TXN-AMOUNT TO WS-RPT-WITHDRAW-TOTAL
                   WHEN "T"
                       SUBTRACT TXN-AMOUNT FROM WS-RUNNING-BAL
                       ADD TXN-AMOUNT TO WS-RPT-TRANSFER-TOTAL
               END-EVALUATE
               ADD 1 TO WS-RPT-TXN-COUNT
               PERFORM FORMAT-AND-PRINT-LINE
           ELSE
               IF TXN-DATE < WS-START-DATE
                   CONTINUE
               END-IF
           END-IF.

       PROCESS-INCOMING-XFER.
           IF TXN-DATE >= WS-START-DATE
               AND TXN-DATE <= WS-END-DATE
               ADD TXN-AMOUNT TO WS-RUNNING-BAL
               ADD TXN-AMOUNT TO WS-RPT-DEPOSIT-TOTAL
               ADD 1 TO WS-RPT-TXN-COUNT
      *        Temporarily change fields for display
               MOVE "D" TO TXN-TYPE
               MOVE "XFER IN" TO TXN-DESC
               PERFORM FORMAT-AND-PRINT-LINE
           END-IF.

       FORMAT-AND-PRINT-LINE.
      *    Format the date YYYY-MM-DD
           MOVE TXN-DATE TO WS-RAW-DATE
           MOVE WS-RAW-DATE(1:4) TO WS-FMT-YEAR
           MOVE WS-RAW-DATE(5:2) TO WS-FMT-MONTH
           MOVE WS-RAW-DATE(7:2) TO WS-FMT-DAY

      *    Format the time HH:MM:SS
           MOVE TXN-TIME TO WS-RAW-TIME
           MOVE WS-RAW-TIME(1:2) TO WS-FMT-HH
           MOVE WS-RAW-TIME(3:2) TO WS-FMT-MM
           MOVE WS-RAW-TIME(5:2) TO WS-FMT-SS

      *    Format amount and balance
           MOVE TXN-AMOUNT TO WS-DISP-AMT
           MOVE WS-RUNNING-BAL TO WS-DISP-BAL

      *    Print the line
           EVALUATE TXN-TYPE
               WHEN "D"
                   DISPLAY "  " WS-FMT-DATE " " WS-FMT-TIME
                       " Deposit  " WS-DISP-AMT
                       " " WS-DISP-BAL
                       " " TXN-DESC
               WHEN "W"
                   DISPLAY "  " WS-FMT-DATE " " WS-FMT-TIME
                       " Withdraw " WS-DISP-AMT
                       " " WS-DISP-BAL
                       " " TXN-DESC
               WHEN "T"
                   DISPLAY "  " WS-FMT-DATE " " WS-FMT-TIME
                       " Transfer " WS-DISP-AMT
                       " " WS-DISP-BAL
                       " " TXN-DESC
           END-EVALUATE.

       PRINT-REPORT-FOOTER.
           DISPLAY "  " WS-SEPARATOR-LINE
           DISPLAY SPACES
           DISPLAY "  STATEMENT SUMMARY"
           DISPLAY "  -----------------"
           DISPLAY "  Transactions:     " WS-RPT-TXN-COUNT
           MOVE WS-RPT-DEPOSIT-TOTAL TO WS-DISP-AMT
           DISPLAY "  Total Deposits:   " WS-DISP-AMT
           MOVE WS-RPT-WITHDRAW-TOTAL TO WS-DISP-AMT
           DISPLAY "  Total Withdrawals:" WS-DISP-AMT
           MOVE WS-RPT-TRANSFER-TOTAL TO WS-DISP-AMT
           DISPLAY "  Total Transfers:  " WS-DISP-AMT
           MOVE WS-RUNNING-BAL TO WS-DISP-BAL
           DISPLAY "  Closing Balance:  " WS-DISP-BAL
           DISPLAY SPACES
           DISPLAY "=================================="
               "=================================="
               "=========="
           DISPLAY "  End of Statement"
           DISPLAY "=================================="
               "=================================="
               "==========".
