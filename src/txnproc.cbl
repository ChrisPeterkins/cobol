       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNPROC.
      *================================================================*
      * TXNPROC - Transaction Processor                                *
      * Interactive program for deposits, withdrawals, and transfers.  *
      * Updates account balances and logs transactions.                *
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
       01  WS-ACCT-STATUS        PIC XX.
           88  WS-ACCT-OK            VALUE "00".
           88  WS-ACCT-NOT-FOUND     VALUE "23".
           88  WS-ACCT-FILE-MISSING  VALUE "35".

       01  WS-TXN-STATUS         PIC XX.
           88  WS-TXN-OK             VALUE "00".
           88  WS-TXN-FILE-MISSING   VALUE "35".

       01  WS-ACCT-OPEN-FLAG     PIC 9 VALUE 0.
           88  WS-ACCT-IS-OPEN       VALUE 1.
       01  WS-TXN-OPEN-FLAG      PIC 9 VALUE 0.
           88  WS-TXN-IS-OPEN        VALUE 1.

       01  WS-MENU-CHOICE        PIC X(1).
           88  WS-DEPOSIT            VALUE "1".
           88  WS-WITHDRAW           VALUE "2".
           88  WS-TRANSFER           VALUE "3".
           88  WS-QUIT               VALUE "Q" "q".

       01  WS-INPUT-ACCTNO       PIC X(8).
       01  WS-INPUT-XFER-ACCTNO  PIC X(8).
       01  WS-INPUT-AMOUNT       PIC X(12).
       01  WS-TXN-AMT            PIC 9(7)V99.
       01  WS-INPUT-DESC         PIC X(20).

       01  WS-NEXT-TXN-ID        PIC 9(10) VALUE 1.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURR-YEAR      PIC 9(4).
           05  WS-CURR-MONTH     PIC 9(2).
           05  WS-CURR-DAY       PIC 9(2).
           05  WS-CURR-HH        PIC 9(2).
           05  WS-CURR-MM        PIC 9(2).
           05  WS-CURR-SS        PIC 9(2).
           05  WS-CURR-REST      PIC X(7).

       01  WS-TODAY-DATE          PIC 9(8).
       01  WS-NOW-TIME            PIC 9(6).

       01  WS-SAVE-ACCT-NO       PIC 9(8).
       01  WS-SAVE-ACCT-NAME     PIC X(30).
       01  WS-SAVE-ACCT-BAL      PIC S9(9)V99.
       01  WS-SAVE-ACCT-TYPE     PIC X(1).
       01  WS-SAVE-ACCT-STAT     PIC X(1).
       01  WS-SAVE-ACCT-DT       PIC 9(8).

       01  WS-NEW-BAL            PIC S9(9)V99.
       01  WS-SIZE-ERR-FLAG      PIC 9 VALUE 0.

       01  WS-TXN-EOF-FLAG       PIC 9 VALUE 0.
           88  WS-TXN-EOF            VALUE 1.

       01  WS-DISP-BAL           PIC $$$,$$$,$$9.99-.
       01  WS-DISP-AMT           PIC $$$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM OPEN-FILES
           IF NOT WS-ACCT-IS-OPEN
               DISPLAY "FATAL: Cannot open account file."
               STOP RUN
           END-IF
           IF NOT WS-TXN-IS-OPEN
               DISPLAY "FATAL: Cannot open transaction file."
               PERFORM CLOSE-FILES
               STOP RUN
           END-IF
           PERFORM FIND-NEXT-TXN-ID
           PERFORM MAIN-MENU UNTIL WS-QUIT
           PERFORM CLOSE-FILES
           STOP RUN.

       OPEN-FILES.
           OPEN I-O ACCT-FILE
           IF WS-ACCT-OK
               SET WS-ACCT-IS-OPEN TO TRUE
           ELSE
               IF WS-ACCT-FILE-MISSING
                   DISPLAY "Account file not found. Run ACCTMGR"
                       " first to create accounts."
               ELSE
                   DISPLAY "Error opening account file: "
                       WS-ACCT-STATUS
               END-IF
               GO TO OPEN-FILES-EXIT
           END-IF

           OPEN EXTEND TXN-FILE
           IF WS-TXN-OK
               SET WS-TXN-IS-OPEN TO TRUE
           ELSE
               IF WS-TXN-FILE-MISSING
                   OPEN OUTPUT TXN-FILE
                   IF WS-TXN-OK
                       SET WS-TXN-IS-OPEN TO TRUE
                   END-IF
               END-IF
           END-IF.
       OPEN-FILES-EXIT.
           EXIT.

       CLOSE-FILES.
           IF WS-ACCT-IS-OPEN
               CLOSE ACCT-FILE
               MOVE 0 TO WS-ACCT-OPEN-FLAG
           END-IF
           IF WS-TXN-IS-OPEN
               CLOSE TXN-FILE
               MOVE 0 TO WS-TXN-OPEN-FLAG
           END-IF.

       FIND-NEXT-TXN-ID.
      *    Reopen txn file for input to find last ID
           IF WS-TXN-IS-OPEN
               CLOSE TXN-FILE
               MOVE 0 TO WS-TXN-OPEN-FLAG
           END-IF
           OPEN INPUT TXN-FILE
           IF WS-TXN-OK
               MOVE 0 TO WS-NEXT-TXN-ID
               MOVE 0 TO WS-TXN-EOF-FLAG
               PERFORM READ-SINGLE-TXN UNTIL WS-TXN-EOF
               ADD 1 TO WS-NEXT-TXN-ID
               CLOSE TXN-FILE
           ELSE
               MOVE 1 TO WS-NEXT-TXN-ID
           END-IF
      *    Reopen in EXTEND mode for appending transactions
           OPEN EXTEND TXN-FILE
           IF WS-TXN-OK
               SET WS-TXN-IS-OPEN TO TRUE
           ELSE
               MOVE 0 TO WS-TXN-OPEN-FLAG
           END-IF.

       READ-SINGLE-TXN.
           READ TXN-FILE
               AT END
                   SET WS-TXN-EOF TO TRUE
               NOT AT END
                   IF TXN-ID > WS-NEXT-TXN-ID
                       MOVE TXN-ID TO WS-NEXT-TXN-ID
                   END-IF
           END-READ.

       GET-CURRENT-DATETIME.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY
               DELIMITED BY SIZE INTO WS-TODAY-DATE
           END-STRING
           STRING WS-CURR-HH WS-CURR-MM WS-CURR-SS
               DELIMITED BY SIZE INTO WS-NOW-TIME
           END-STRING.

       MAIN-MENU.
           DISPLAY SPACES
           DISPLAY "========================================="
           DISPLAY "  COBOL BANK LEDGER - TRANSACTION PROC"
           DISPLAY "========================================="
           DISPLAY "  1. Deposit"
           DISPLAY "  2. Withdraw"
           DISPLAY "  3. Transfer"
           DISPLAY "  Q. Quit"
           DISPLAY "========================================="
           DISPLAY "Enter choice: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           EVALUATE TRUE
               WHEN WS-DEPOSIT
                   PERFORM PROCESS-DEPOSIT
               WHEN WS-WITHDRAW
                   PERFORM PROCESS-WITHDRAWAL
               WHEN WS-TRANSFER
                   PERFORM PROCESS-TRANSFER
               WHEN WS-QUIT
                   DISPLAY "Goodbye."
               WHEN OTHER
                   DISPLAY "Invalid choice. Try again."
           END-EVALUATE.

       PROCESS-DEPOSIT.
           DISPLAY SPACES
           DISPLAY "--- Deposit ---"
           DISPLAY "Account number (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCTNO
           MOVE WS-INPUT-ACCTNO TO ACCT-NO

           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Account not found."
                   GO TO PROCESS-DEPOSIT-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               DISPLAY "Account is not active."
               GO TO PROCESS-DEPOSIT-EXIT
           END-IF

           DISPLAY "Deposit amount:"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMOUNT
           MOVE FUNCTION NUMVAL(WS-INPUT-AMOUNT) TO WS-TXN-AMT
           IF WS-TXN-AMT < 0.01
               DISPLAY "Amount must be at least $0.01."
               GO TO PROCESS-DEPOSIT-EXIT
           END-IF

           DISPLAY "Description (up to 20 chars):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-DESC

           MOVE 0 TO WS-SIZE-ERR-FLAG
           ADD WS-TXN-AMT TO ACCT-BAL
               ON SIZE ERROR
                   MOVE 1 TO WS-SIZE-ERR-FLAG
           END-ADD

           IF WS-SIZE-ERR-FLAG = 1
               DISPLAY "ERROR: Balance overflow. Deposit too "
                   "large."
               GO TO PROCESS-DEPOSIT-EXIT
           END-IF

           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               DISPLAY "ERROR: Could not update account."
               GO TO PROCESS-DEPOSIT-EXIT
           END-IF

           PERFORM GET-CURRENT-DATETIME
           MOVE WS-NEXT-TXN-ID    TO TXN-ID
           MOVE ACCT-NO            TO TXN-ACCT-NO
           MOVE "D"                TO TXN-TYPE
           MOVE WS-TXN-AMT        TO TXN-AMOUNT
           MOVE WS-TODAY-DATE      TO TXN-DATE
           MOVE WS-NOW-TIME        TO TXN-TIME
           MOVE WS-INPUT-DESC      TO TXN-DESC
           MOVE "C"                TO TXN-STATUS
           MOVE ZEROS              TO TXN-XFER-ACCT

           WRITE TXN-REC
           IF WS-TXN-OK
               ADD 1 TO WS-NEXT-TXN-ID
               MOVE WS-TXN-AMT TO WS-DISP-AMT
               MOVE ACCT-BAL TO WS-DISP-BAL
               DISPLAY "Deposit successful!"
               DISPLAY "  Amount:      " WS-DISP-AMT
               DISPLAY "  New Balance: " WS-DISP-BAL
           ELSE
               DISPLAY "WARNING: Account updated but "
                   "transaction log failed."
           END-IF.
       PROCESS-DEPOSIT-EXIT.
           EXIT.

       PROCESS-WITHDRAWAL.
           DISPLAY SPACES
           DISPLAY "--- Withdrawal ---"
           DISPLAY "Account number (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCTNO
           MOVE WS-INPUT-ACCTNO TO ACCT-NO

           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Account not found."
                   GO TO PROCESS-WITHDRAWAL-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               DISPLAY "Account is not active."
               GO TO PROCESS-WITHDRAWAL-EXIT
           END-IF

           MOVE ACCT-BAL TO WS-DISP-BAL
           DISPLAY "Current balance: " WS-DISP-BAL
           DISPLAY "Withdrawal amount:"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMOUNT
           MOVE FUNCTION NUMVAL(WS-INPUT-AMOUNT) TO WS-TXN-AMT
           IF WS-TXN-AMT < 0.01
               DISPLAY "Amount must be at least $0.01."
               GO TO PROCESS-WITHDRAWAL-EXIT
           END-IF

           IF WS-TXN-AMT > ACCT-BAL
               DISPLAY "Insufficient funds. No overdraft "
                   "allowed."
               GO TO PROCESS-WITHDRAWAL-EXIT
           END-IF

           DISPLAY "Description (up to 20 chars):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-DESC

           SUBTRACT WS-TXN-AMT FROM ACCT-BAL
               ON SIZE ERROR
                   DISPLAY "ERROR: Balance underflow."
                   GO TO PROCESS-WITHDRAWAL-EXIT
           END-SUBTRACT

           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               DISPLAY "ERROR: Could not update account."
               GO TO PROCESS-WITHDRAWAL-EXIT
           END-IF

           PERFORM GET-CURRENT-DATETIME
           MOVE WS-NEXT-TXN-ID    TO TXN-ID
           MOVE ACCT-NO            TO TXN-ACCT-NO
           MOVE "W"                TO TXN-TYPE
           MOVE WS-TXN-AMT        TO TXN-AMOUNT
           MOVE WS-TODAY-DATE      TO TXN-DATE
           MOVE WS-NOW-TIME        TO TXN-TIME
           MOVE WS-INPUT-DESC      TO TXN-DESC
           MOVE "C"                TO TXN-STATUS
           MOVE ZEROS              TO TXN-XFER-ACCT

           WRITE TXN-REC
           IF WS-TXN-OK
               ADD 1 TO WS-NEXT-TXN-ID
               MOVE WS-TXN-AMT TO WS-DISP-AMT
               MOVE ACCT-BAL TO WS-DISP-BAL
               DISPLAY "Withdrawal successful!"
               DISPLAY "  Amount:      " WS-DISP-AMT
               DISPLAY "  New Balance: " WS-DISP-BAL
           ELSE
               DISPLAY "WARNING: Account updated but "
                   "transaction log failed."
           END-IF.
       PROCESS-WITHDRAWAL-EXIT.
           EXIT.

       PROCESS-TRANSFER.
           DISPLAY SPACES
           DISPLAY "--- Transfer ---"
           DISPLAY "From account number (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCTNO
           MOVE WS-INPUT-ACCTNO TO ACCT-NO

           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Source account not found."
                   GO TO PROCESS-TRANSFER-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               DISPLAY "Source account is not active."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

      *    Save source account data
           MOVE ACCT-NO     TO WS-SAVE-ACCT-NO
           MOVE ACCT-NAME   TO WS-SAVE-ACCT-NAME
           MOVE ACCT-BAL    TO WS-SAVE-ACCT-BAL
           MOVE ACCT-TYPE   TO WS-SAVE-ACCT-TYPE
           MOVE ACCT-STATUS TO WS-SAVE-ACCT-STAT
           MOVE ACCT-OPEN-DT TO WS-SAVE-ACCT-DT

           DISPLAY "To account number (8 digits):"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-XFER-ACCTNO
           IF WS-INPUT-XFER-ACCTNO = WS-INPUT-ACCTNO
               DISPLAY "Cannot transfer to the same account."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

           MOVE WS-INPUT-XFER-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "Target account not found."
                   GO TO PROCESS-TRANSFER-EXIT
           END-READ

           IF NOT ACCT-IS-ACTIVE
               DISPLAY "Target account is not active."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

      *    Restore source account for display
           MOVE WS-SAVE-ACCT-BAL TO WS-DISP-BAL
           DISPLAY "Source balance: " WS-DISP-BAL
           DISPLAY "Transfer amount:"
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMOUNT
           MOVE FUNCTION NUMVAL(WS-INPUT-AMOUNT) TO WS-TXN-AMT
           IF WS-TXN-AMT < 0.01
               DISPLAY "Amount must be at least $0.01."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

           IF WS-TXN-AMT > WS-SAVE-ACCT-BAL
               DISPLAY "Insufficient funds in source account."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

      *    Update source account (withdraw)
           MOVE WS-SAVE-ACCT-NO   TO ACCT-NO
           MOVE WS-SAVE-ACCT-NAME TO ACCT-NAME
           SUBTRACT WS-TXN-AMT FROM WS-SAVE-ACCT-BAL
               GIVING ACCT-BAL
           MOVE WS-SAVE-ACCT-TYPE TO ACCT-TYPE
           MOVE WS-SAVE-ACCT-STAT TO ACCT-STATUS
           MOVE WS-SAVE-ACCT-DT   TO ACCT-OPEN-DT
           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               DISPLAY "ERROR: Could not update source account."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

      *    Update target account (deposit)
           MOVE WS-INPUT-XFER-ACCTNO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   DISPLAY "CRITICAL: Target account vanished!"
                   GO TO PROCESS-TRANSFER-EXIT
           END-READ
           ADD WS-TXN-AMT TO ACCT-BAL
               ON SIZE ERROR
                   DISPLAY "ERROR: Target balance overflow."
                   GO TO PROCESS-TRANSFER-EXIT
           END-ADD
           REWRITE ACCT-REC
           IF NOT WS-ACCT-OK
               DISPLAY "ERROR: Could not update target account."
               GO TO PROCESS-TRANSFER-EXIT
           END-IF

      *    Log the transfer transaction
           PERFORM GET-CURRENT-DATETIME
           MOVE WS-NEXT-TXN-ID         TO TXN-ID
           MOVE WS-SAVE-ACCT-NO        TO TXN-ACCT-NO
           MOVE "T"                     TO TXN-TYPE
           MOVE WS-TXN-AMT             TO TXN-AMOUNT
           MOVE WS-TODAY-DATE           TO TXN-DATE
           MOVE WS-NOW-TIME             TO TXN-TIME
           MOVE "TRANSFER"              TO TXN-DESC
           MOVE "C"                     TO TXN-STATUS
           MOVE WS-INPUT-XFER-ACCTNO   TO TXN-XFER-ACCT

           WRITE TXN-REC
           IF WS-TXN-OK
               ADD 1 TO WS-NEXT-TXN-ID
               MOVE WS-TXN-AMT TO WS-DISP-AMT
               DISPLAY "Transfer successful!"
               DISPLAY "  Amount: " WS-DISP-AMT
               SUBTRACT WS-TXN-AMT FROM WS-SAVE-ACCT-BAL
                   GIVING WS-NEW-BAL
               ADD WS-TXN-AMT TO WS-SAVE-ACCT-BAL
                   GIVING WS-NEW-BAL
      *        Show final balances by re-reading accounts
               MOVE WS-SAVE-ACCT-NO TO ACCT-NO
               READ ACCT-FILE
                   INVALID KEY CONTINUE
               END-READ
               MOVE ACCT-BAL TO WS-DISP-BAL
               DISPLAY "  From " ACCT-NO " balance: "
                   WS-DISP-BAL
               MOVE WS-INPUT-XFER-ACCTNO TO ACCT-NO
               READ ACCT-FILE
                   INVALID KEY CONTINUE
               END-READ
               MOVE ACCT-BAL TO WS-DISP-BAL
               DISPLAY "  To   " ACCT-NO " balance: "
                   WS-DISP-BAL
           ELSE
               DISPLAY "WARNING: Transfer done but "
                   "transaction log failed."
           END-IF.
       PROCESS-TRANSFER-EXIT.
           EXIT.
