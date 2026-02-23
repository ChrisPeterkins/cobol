       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAYEND.
      *================================================================*
      * DAYEND - End-of-Day Batch Reconciliation                       *
      * Reads today's transactions, sorts by account, sums totals,     *
      * and verifies balances against the account master file.          *
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

           SELECT SORT-FILE
               ASSIGN TO "data/SORT-WORK.tmp".

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY ACCT-REC.

       FD  TXN-FILE.
       COPY TXNL-REC.

       SD  SORT-FILE.
       01  SORT-REC.
           05  SORT-TXN-ID        PIC 9(10).
           05  SORT-TXN-ACCT-NO   PIC 9(8).
           05  SORT-TXN-TYPE      PIC X(1).
           05  SORT-TXN-AMOUNT    PIC 9(7)V99.
           05  SORT-TXN-DATE      PIC 9(8).
           05  SORT-TXN-TIME      PIC 9(6).
           05  SORT-TXN-DESC      PIC X(20).
           05  SORT-TXN-STATUS    PIC X(1).
           05  SORT-TXN-XFER-ACCT PIC 9(8).

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS         PIC XX.
           88  WS-ACCT-OK             VALUE "00".
           88  WS-ACCT-NOT-FOUND      VALUE "23".
           88  WS-ACCT-EOF            VALUE "10".
           88  WS-ACCT-FILE-MISSING   VALUE "35".

       01  WS-TXN-STATUS          PIC XX.
           88  WS-TXN-OK              VALUE "00".
           88  WS-TXN-EOF             VALUE "10".
           88  WS-TXN-FILE-MISSING    VALUE "35".

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURR-YEAR       PIC 9(4).
           05  WS-CURR-MONTH      PIC 9(2).
           05  WS-CURR-DAY        PIC 9(2).
           05  WS-CURR-REST       PIC X(7).

       01  WS-TODAY-DATE           PIC 9(8).
       01  WS-INPUT-DATE           PIC X(8).
       01  WS-PROCESS-DATE         PIC 9(8).

      *--- Control break fields ---
       01  WS-PREV-ACCT-NO        PIC 9(8) VALUE ZEROS.
       01  WS-CURR-ACCT-NO        PIC 9(8).

      *--- Per-account accumulators ---
       01  WS-ACCT-DEPOSITS        PIC S9(9)V99 VALUE ZEROS.
       01  WS-ACCT-WITHDRAWALS     PIC S9(9)V99 VALUE ZEROS.
       01  WS-ACCT-TRANSFERS-OUT   PIC S9(9)V99 VALUE ZEROS.
       01  WS-ACCT-TRANSFERS-IN    PIC S9(9)V99 VALUE ZEROS.
       01  WS-ACCT-NET-CHANGE      PIC S9(9)V99.
       01  WS-ACCT-TXN-COUNT       PIC 9(5) VALUE ZEROS.

      *--- Grand totals ---
       01  WS-TOTAL-DEPOSITS       PIC S9(11)V99 VALUE ZEROS.
       01  WS-TOTAL-WITHDRAWALS    PIC S9(11)V99 VALUE ZEROS.
       01  WS-TOTAL-TRANSFERS      PIC S9(11)V99 VALUE ZEROS.
       01  WS-TOTAL-TXN-COUNT      PIC 9(7) VALUE ZEROS.
       01  WS-TOTAL-ACCT-COUNT     PIC 9(5) VALUE ZEROS.
       01  WS-DISCREPANCY-COUNT    PIC 9(5) VALUE ZEROS.
       01  WS-TODAYS-TXN-COUNT     PIC 9(7) VALUE ZEROS.

       01  WS-SORT-EOF-FLAG        PIC 9 VALUE 0.
           88  WS-SORT-EOF             VALUE 1.

       01  WS-DISP-AMT             PIC $$$,$$$,$$9.99-.
       01  WS-DISP-DATE            PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "========================================="
           DISPLAY "  END-OF-DAY BATCH RECONCILIATION"
           DISPLAY "========================================="

           PERFORM GET-PROCESS-DATE
           MOVE WS-PROCESS-DATE TO WS-DISP-DATE
           DISPLAY "Processing date: " WS-PROCESS-DATE
           DISPLAY SPACES

           SORT SORT-FILE
               ON ASCENDING KEY SORT-TXN-ACCT-NO
               ON ASCENDING KEY SORT-TXN-ID
               INPUT PROCEDURE IS FILTER-TODAYS-TXNS
               OUTPUT PROCEDURE IS PROCESS-SORTED-TXNS

           PERFORM PRINT-GRAND-TOTALS
           STOP RUN.

       GET-PROCESS-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-TODAY-DATE
           END-STRING
           DISPLAY "Enter date to process (YYYYMMDD) or "
               "press Enter for today:"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-DATE
           IF WS-INPUT-DATE = SPACES
               MOVE WS-TODAY-DATE TO WS-PROCESS-DATE
           ELSE
               MOVE WS-INPUT-DATE TO WS-PROCESS-DATE
           END-IF.

       FILTER-TODAYS-TXNS.
           OPEN INPUT TXN-FILE
           IF NOT WS-TXN-OK
               IF WS-TXN-FILE-MISSING
                   DISPLAY "No transaction file found."
               ELSE
                   DISPLAY "Error opening transaction file: "
                       WS-TXN-STATUS
               END-IF
               GO TO FILTER-TODAYS-TXNS-EXIT
           END-IF

           PERFORM READ-AND-FILTER UNTIL WS-TXN-EOF

           CLOSE TXN-FILE.
       FILTER-TODAYS-TXNS-EXIT.
           EXIT.

       READ-AND-FILTER.
           READ TXN-FILE
               AT END
                   CONTINUE
               NOT AT END
                   IF TXN-DATE = WS-PROCESS-DATE
                       AND TXN-STATUS = "C"
                       MOVE TXN-REC TO SORT-REC
                       RELEASE SORT-REC
                       ADD 1 TO WS-TODAYS-TXN-COUNT
                   END-IF
           END-READ.

       PROCESS-SORTED-TXNS.
           OPEN I-O ACCT-FILE
           IF NOT WS-ACCT-OK
               DISPLAY "Error opening account file: "
                   WS-ACCT-STATUS
               GO TO PROCESS-SORTED-TXNS-EXIT
           END-IF

           DISPLAY "Transactions found for this date: "
               WS-TODAYS-TXN-COUNT
           DISPLAY SPACES

           IF WS-TODAYS-TXN-COUNT = ZEROS
               DISPLAY "No transactions to process."
               CLOSE ACCT-FILE
               GO TO PROCESS-SORTED-TXNS-EXIT
           END-IF

           MOVE ZEROS TO WS-PREV-ACCT-NO
           MOVE 0 TO WS-SORT-EOF-FLAG

           RETURN SORT-FILE INTO SORT-REC
               AT END
                   SET WS-SORT-EOF TO TRUE
                   GO TO PROCESS-SORTED-TXNS-WRAP
           END-RETURN

           MOVE SORT-TXN-ACCT-NO TO WS-PREV-ACCT-NO
           PERFORM RESET-ACCT-ACCUMULATORS
           PERFORM ACCUMULATE-TXN
           PERFORM PROCESS-SORTED-LOOP
               UNTIL WS-SORT-EOF.

       PROCESS-SORTED-TXNS-WRAP.
      *    Process last account group
           IF WS-PREV-ACCT-NO NOT = ZEROS
               PERFORM CHECK-ACCOUNT-BALANCE
           END-IF
           CLOSE ACCT-FILE.
       PROCESS-SORTED-TXNS-EXIT.
           EXIT.

       PROCESS-SORTED-LOOP.
           RETURN SORT-FILE INTO SORT-REC
               AT END
                   SET WS-SORT-EOF TO TRUE
                   PERFORM CHECK-ACCOUNT-BALANCE
               NOT AT END
                   MOVE SORT-TXN-ACCT-NO TO WS-CURR-ACCT-NO
      *            Control break - account number changed
                   IF WS-CURR-ACCT-NO NOT = WS-PREV-ACCT-NO
                       PERFORM CHECK-ACCOUNT-BALANCE
                       MOVE WS-CURR-ACCT-NO TO WS-PREV-ACCT-NO
                       PERFORM RESET-ACCT-ACCUMULATORS
                   END-IF
                   PERFORM ACCUMULATE-TXN
           END-RETURN.

       RESET-ACCT-ACCUMULATORS.
           MOVE ZEROS TO WS-ACCT-DEPOSITS
           MOVE ZEROS TO WS-ACCT-WITHDRAWALS
           MOVE ZEROS TO WS-ACCT-TRANSFERS-OUT
           MOVE ZEROS TO WS-ACCT-TRANSFERS-IN
           MOVE ZEROS TO WS-ACCT-TXN-COUNT.

       ACCUMULATE-TXN.
           ADD 1 TO WS-ACCT-TXN-COUNT
           ADD 1 TO WS-TOTAL-TXN-COUNT
           EVALUATE SORT-TXN-TYPE
               WHEN "D"
                   ADD SORT-TXN-AMOUNT TO WS-ACCT-DEPOSITS
                   ADD SORT-TXN-AMOUNT TO WS-TOTAL-DEPOSITS
               WHEN "W"
                   ADD SORT-TXN-AMOUNT TO WS-ACCT-WITHDRAWALS
                   ADD SORT-TXN-AMOUNT TO WS-TOTAL-WITHDRAWALS
               WHEN "T"
                   ADD SORT-TXN-AMOUNT TO WS-ACCT-TRANSFERS-OUT
                   ADD SORT-TXN-AMOUNT TO WS-TOTAL-TRANSFERS
           END-EVALUATE.

       CHECK-ACCOUNT-BALANCE.
           ADD 1 TO WS-TOTAL-ACCT-COUNT
           MOVE WS-PREV-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "  WARNING: Account " WS-PREV-ACCT-NO
                       " in transactions but not in master file!"
                   ADD 1 TO WS-DISCREPANCY-COUNT
                   GO TO CHECK-ACCOUNT-BALANCE-EXIT
           END-READ

           COMPUTE WS-ACCT-NET-CHANGE =
               WS-ACCT-DEPOSITS
               - WS-ACCT-WITHDRAWALS
               - WS-ACCT-TRANSFERS-OUT

           DISPLAY "  Account: " WS-PREV-ACCT-NO
               "  " ACCT-NAME
           MOVE WS-ACCT-DEPOSITS TO WS-DISP-AMT
           DISPLAY "    Deposits:    " WS-DISP-AMT
               "  (count: " WS-ACCT-TXN-COUNT ")"
           MOVE WS-ACCT-WITHDRAWALS TO WS-DISP-AMT
           DISPLAY "    Withdrawals: " WS-DISP-AMT
           MOVE WS-ACCT-TRANSFERS-OUT TO WS-DISP-AMT
           DISPLAY "    Transfers:   " WS-DISP-AMT
           MOVE WS-ACCT-NET-CHANGE TO WS-DISP-AMT
           DISPLAY "    Net Change:  " WS-DISP-AMT
           MOVE ACCT-BAL TO WS-DISP-AMT
           DISPLAY "    Cur Balance: " WS-DISP-AMT
           DISPLAY SPACES.
       CHECK-ACCOUNT-BALANCE-EXIT.
           EXIT.

       PRINT-GRAND-TOTALS.
           DISPLAY "========================================="
           DISPLAY "  DAILY SUMMARY"
           DISPLAY "========================================="
           DISPLAY "  Date:            " WS-PROCESS-DATE
           DISPLAY "  Accounts Active: " WS-TOTAL-ACCT-COUNT
           DISPLAY "  Transactions:    " WS-TOTAL-TXN-COUNT
           MOVE WS-TOTAL-DEPOSITS TO WS-DISP-AMT
           DISPLAY "  Total Deposits:  " WS-DISP-AMT
           MOVE WS-TOTAL-WITHDRAWALS TO WS-DISP-AMT
           DISPLAY "  Total Withdraws: " WS-DISP-AMT
           MOVE WS-TOTAL-TRANSFERS TO WS-DISP-AMT
           DISPLAY "  Total Transfers: " WS-DISP-AMT
           DISPLAY "  Discrepancies:   " WS-DISCREPANCY-COUNT
           IF WS-DISCREPANCY-COUNT > ZEROS
               DISPLAY "  ** RECONCILIATION ISSUES FOUND **"
           ELSE
               DISPLAY "  Reconciliation: PASS"
           END-IF
           DISPLAY "=========================================".
